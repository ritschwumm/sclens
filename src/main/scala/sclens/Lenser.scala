package sclens

import scala.language.experimental.macros
import scala.reflect.macros.Context

import scutil.lang._
import scutil.Implicits._
import scutil.tried._

/** creates lens instances for a case classes' fields */
object Lenser {
	// NOTE without the TypeTag T gets passed as Nothing
	import reflect.runtime.universe._
	def apply[T:TypeTag]	= new Lenser[T]
}

final class Lenser[T] extends Dynamic {
	/*
	def selectDynamic(propName:String)	= macro LenserImpl.selectDynamic[T]
	def applyDynamic(propName:String)()	= macro LenserImpl.applyDynamic[T]
	*/
	
	def selectDynamic(propName:String)	= macro LenserImpl.selectDynamic[T]
}

object LenserImpl {
	/*
	def selectDynamic[T:c.WeakTypeTag](c:Context)(propName:c.Expr[String])	=
			applyDynamic[T](c)(propName)()
	
	def applyDynamic[T:c.WeakTypeTag](c:Context)(propName:c.Expr[String])()	= {
	*/
	
	def selectDynamic[T:c.WeakTypeTag](c:Context)(propName:c.Expr[String])	= {
		import c.universe._
		
		def mkLens(containerName:TermName, containerType:Type, valueName:TermName, valueType:Type, fieldName:TermName)	=
				Apply(
					TypeApply(
						multiSelect("scutil", "lang", "Lens", "apply"),
						List(
							TypeTree(containerType),
							TypeTree(valueType)
						)
					),
					List(
						mkGetter(containerName, containerType, fieldName),
						mkSetter(containerName, containerType, valueName, valueType, fieldName)
					)
				)
				
		def mkGetter(containerName:TermName, containerType:Type, fieldName:TermName)	=
				Function(
					List(
						mkParam(containerName, containerType)
					),
					mkAccess(containerName, fieldName)
				)
				
		def mkSetter(containerName:TermName, containerType:Type, valueName:TermName, valueType:Type, fieldName:TermName)	=
				Function(
					List(
						mkParam(containerName,	containerType),
						mkParam(valueName,		valueType)
					),
					Apply(
						mkAccess(containerName, "copy"),
						List(
							AssignOrNamedArg(
								Ident(fieldName),
								Ident(valueName)
							)
						 )
					)
				)
				
		def mkParam(name:TermName, tpe:Type)	=
				ValDef(
					Modifiers(Flag.PARAM),
					name, 
					TypeTree(tpe),
					EmptyTree
				)
				
		def mkAccess(a:TermName, b:TermName)	=
				Select(
					Ident(a), 
					b
				)
				
		def multiSelect(start:String, names:String*):RefTree	=
				multiSelect1(Ident(newTermName(start)), names:_*)
			
		def multiSelect1(start:RefTree, names:String*):RefTree	=
				(names foldLeft start) { (last:RefTree, name:String) => 
					Select(last, newTermName(name)) 
				}
				
		val out	=
				for {
					name	<- 
							propName.tree										matchOption 
							{ case Literal(Constant(name:String))	=> name }	toWin
							s"unexpected propName: ${propName.tree}"
					containerTpe	= c.weakTypeOf[T]
					member	<-
							containerTpe		member 
							newTermName(name)	guardBy 
							{ _ != NoSymbol }	toWin 
							s"value ${name} is not a member of ${containerTpe}"
					valueTpe	<-
							member									typeSignatureIn
							containerTpe							matchOption
							{ case NullaryMethodType(tpe) => tpe }	toWin
							s"member ${name} of ${containerTpe} is not a field"
				}
				yield mkLens("c$", containerTpe, "v$", valueTpe, name)
				
		out cataSwapped (
				it	=> c.Expr[Any](c resetAllAttrs it),
				it	=> c abort (c.enclosingPosition, it))
	}
}
