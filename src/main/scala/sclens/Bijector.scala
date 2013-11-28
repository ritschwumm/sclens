package sclens

import scala.language.experimental.macros
import scala.reflect.macros.Context

import scutil.lang._
import scutil.Implicits._

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
							
					unapplySignature	<-
							unapplySingle match {
								case t @ TypeRef(_, _, _)	=> Win(t)
								case x						=> Fail(s"unexpected unapply return ${x}")
							}
							
					applySymbol		<- getDeclaration(companionType, "apply")
					applyMethods	= applySymbol.asMethod.alternatives
					
					// (method, raw signature)
					applyMethods0	=
							for {
								method	<- applyMethods collect { case (method:MethodSymbol) => method }
								params	<- method.paramss.singleOption
							}
							yield (
								method, 
								params map { _.asTerm.typeSignature }
							)
							
					// (method, flat signature)
					applyMethods1	=
							applyMethods0 filter { case (applyMethod, applySignature) =>
								applySignature.size match {
									case 0	=> false	// should not happen
									case 1	=> applySignature == List(unapplySignature)
									case n	=> applySignature == unapplySignature.args &&
											// TODO make sure this doesn't fail for case classes somehow
											unapplySignature <:< typeOf[Product]
								}
							}
							
					applyTmp		<-
							applyMethods1
							.singleOption
							.toWin (s"expected a single apply method matching unapply's types")
							
					(applyMethod, applySignature)	
									= applyTmp
							
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
