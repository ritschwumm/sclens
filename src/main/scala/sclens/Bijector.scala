package sclens

import scala.language.experimental.macros
import scala.reflect.macros.Context

import scutil.lang._
import scutil.Implicits._
import scutil.tried._

/** creates bijections from the apply/unapply methods in a case classes' companion object */
object Bijector {
	import reflect.runtime.universe._
	
	def apply[T]	= macro applyImpl[T]
	
	def applyImpl[T:c.WeakTypeTag](c:Context):c.Expr[Any]	= {
		import c.universe._
		
		val selfType	= weakTypeOf[T]
		
		def getCompanion(symbol:Symbol)	=
				symbol.companionSymbol	preventBy
				{ _ == NoSymbol }		toWin
				s"unexpected NoSymbol for companion of ${symbol}"
		
		def getDeclaration(typ:Type, name:String):Tried[String,Symbol]	=
				typ					declaration
			 	newTermName(name)	preventBy 
			 	{ _ == NoSymbol }	toWin 
			 	s"unexpected NoSymbol for companion declaration ${name} of type ${typ}"
				
		def mkParam(name:TermName, tpe:Type)	=
				ValDef(
					Modifiers(Flag.PARAM),
					name, 
					TypeTree(tpe),
					EmptyTree
				)
				
		def multiSelect(start:String, names:String*):RefTree	=
				multiSelect1(Ident(newTermName(start)), names:_*)
			
		def multiSelect1(start:RefTree, names:String*):RefTree	=
				(names foldLeft start) { (last:RefTree, name:String) => 
					Select(last, newTermName(name)) 
				}
		
		val out	=
				for {
					companionSymbol	<- getCompanion(selfType.typeSymbol)
					companionType	= companionSymbol.typeSignature
					
					unapplySymbol	<- getDeclaration(companionType, "unapply")
					unapplyMethod	= unapplySymbol.asMethod
					unapplyReturn	= unapplyMethod.returnType
					
					// List(Int)	List((Int, Short))
					unapplyRef		<- 
							unapplyReturn 
							.matchOption	{ case TypeRef(_, _, args) => args }	
							.toWin			(s"expected unapply TypeRef")
					// Int			(Int,Short)
					unapplySingle	<- 
							unapplyRef
							.singleOption 
							.toWin	(s"expected unapply Option to return a single value")
							
					// List(Int)	List(Int, Short)
					applySignature	<-
							unapplySingle match {
								// NOTE nested case classes implement Product, too
								case t @ TypeRef(_, _, args)	
								if t <:< typeOf[Product] && args.nonEmpty		=> Win(args)
								case t @ TypeRef(_, _, _)						=> Win(List(t))
								case x											=> Fail(s"unexpected unapply return ${x}")
							}
					
					applySymbol		<- getDeclaration(companionType, "apply")
					applyMethods	= applySymbol.asMethod.alternatives
					
					applyMethods1	=
							applyMethods 
							.collect {
								 case (method:MethodSymbol) 
								 if method.paramss.singleOption map { _ map { _.asTerm.typeSignature } } exists { _ == applySignature }
								 => method
							}
							
					applyMethod		<-
							applyMethods1
							.singleOption
							.toWin (s"expected a single apply method matching unapply's types")
							
					_				<- 
							(applyMethod.paramss.size == 1)
							.tried (s"expected apply to have a single parameter list", ())
							
					// write	unapply		call get	T=>(...)
					writeFunc	=  
							Function(
								List(
									mkParam("it", selfType)
								),
								Select(
									Apply(
										Select(Ident(companionSymbol), newTermName("unapply")),
										List(Ident(newTermName("it")))
									),
									newTermName("get")
								)
							)
							
					// read		apply		tuple input	(...)=>T
					readFunc	=  
							Function(
								List(
									mkParam("it", unapplySingle)
								),
								Apply(
									Select(Ident(companionSymbol), newTermName("apply")),
									if (applySignature.size == 1) {
										List(
											Ident(newTermName("it"))
										)
									}
									else {
										(1 to applySignature.size).toList map { i =>
											multiSelect("it", "_"+i)
										}
									}
								)
							)
							
					lensCreate	=
							// BETTER typeApply?
							Apply(
								multiSelect("scutil", "lang", "Bijection", "apply"),
								List(
									writeFunc,
									readFunc
								)
							)
				}
				yield lensCreate
				
		out cata (
				it	=> c abort (c.enclosingPosition, it),
				it	=> c.Expr[Any](c resetAllAttrs it))
	}
}
