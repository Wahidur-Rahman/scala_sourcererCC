package visitors

import utils.SortFile

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.meta._

/** Reference URL:https://astexplorer.net/#/gist/22cf8a3fcb2155c087ae94b4d194c1b6/d10c646ecfae4c69c919408aa3aaefb2deda2df7
  * This class traverses the abstract syntax tree to extract the representation 1&2
  */
class Visitor {
  var identifierStack=new Stack[String]()   //to deal with function nesting,temporarily stores the value of a terminal node for one or more methods
  var ASTStack=new Stack[String]()   //to deal with function nesting,temporarily stores the value of a path node for one or more methods
  var begin=false                                //determine whether it is in a method body
  var beginClass=false
  var temName=""
  var functionStr=""
  var temClass=""
  var methodName=""         //methodName
  var tempPkg=""
  var identifierResult = ""                        //  the output of leaf node
  var ASTResult=""                                 // the output of path node
  val sourceMethodCode=new ArrayBuffer[String]()   //record the source code of each method
  val sourceFileName=new ArrayBuffer[String]()     //recode the source file of each method
  def visitCtor(ctor: Ctor):Unit={
    ctor match {
      case y:Ctor.Primary=>visitCtorPrimary(y)
      case y:Ctor.Secondary=>visitCtorSecondary(y)
      case _=>println("visit Ctor"+ctor.toString())
    }
  }
  def visitCtorPrimary(primary: Ctor.Primary):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"CtorPrimary ")
    }
    visitChild(primary)
  }
  def visitTerm(term: Term):Unit={
    term match {
      case y:Term.Block=>visitTermBlock(y)
      case y:Term.Annotate=>visitTermAnnotate(y)
      case y:Term.Apply=>visitTermApply(y)
      case y:Term.ApplyInfix=>visitTermApplyInfix(y)
      case y:Term.ApplyType=>visitTermApplyType(y)
      case y:Term.ApplyUnary=>visitTermApplyUnary(y)
      case y:Term.Ascribe=>visitTermAscribe(y)
      case y:Term.Assign=>visitTermAssign(y)
      case y:Term.Do=>visitTermDo(y)
      case y:Term.Eta=>visitTermEta(y)
      case y:Term.For=>visitTermFor(y)
      case y:Term.ForYield=>visitTermForYield(y)
      case y:Term.Function=>visitTermFunction(y)
      case y:Term.If=>visitTermIf(y)
      case y:Term.Interpolate=>visitTermInterpolate(y)
      case y:Term.Match=>visitTermMatch(y)
      case y:Term.Name=>visitTermName(y)
      case y:Term.New=>visitTermNew(y)
      case y:Term.NewAnonymous=>visitTermNewAnonymous(y)
      case y:Term.Param=>visitTermParam(y)
      case y:Term.PartialFunction=>visitTermPartialFunction(y)
      case y:Term.Placeholder=>visitTermPlaceholder(y)
      case y:Term.Ref=>visitTermRef(y)
      case y:Term.Repeated=>visitTermRepeated(y)
      case y:Term.Return=>visitTermReturn(y)
      case y:Term.Select=>visitTermSelect(y)
      case y:Term.Super=>visitTermSuper(y)
      case y:Term.This=>visitTermThis(y)
      case y:Term.Throw=>visitTermThrow(y)
      case y:Term.TryWithHandler=>visitTermTryWithHandler(y)
      case y:Term.Try=>visitTermTry(y)
      case y:Term.Tuple=>visitTermTuple(y)
      case y:Term.While=>visitTermWhile(y)
      case y:Term.Xml=>visitTermXml(y)
      case _=>println("visitTerm"+term.toString())
    }
  }
  def visitTermTry(value: Term.Try):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermTry ")
    }
    visitChild(value)
  }
  def visitTermAnnotate(annotate: Term.Annotate):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermAnnotate ")
    }
    visitChild(annotate)
  }
  def visitTermApply(apply: Term.Apply):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermApply ")
    }
    visitChild(apply)
  }
  def visitTermApplyInfix(infix: Term.ApplyInfix):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermApplyInfix ")
    }
    visitChild(infix)
  }
  def visitTermApplyType(applyType: Term.ApplyType):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermApplyType ")
    }
    visitChild(applyType)
  }
  def visitTermApplyUnary(unary: Term.ApplyUnary):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermApplyUnary ")
    }
    visitChild(unary)
  }
  def visitTermAscribe(ascribe: Term.Ascribe):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermAscribe ")
    }
    visitChild(ascribe)
  }
  def visitTermAssign(assign: Term.Assign):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermAssign ")
    }
    visitChild(assign)
  }
  def visitTermBlock(block: Term.Block):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermBlock ")
    }
    visitChild(block)
  }
  def visitTermDo(value: Term.Do):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermDo ")
    }
    visitChild(value)
  }
  def visitCtorSecondary(secondary: Ctor.Secondary):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"CtorSecondary ")
    }
    visitChild(secondary)
  }
  def visitTermEta(eta: Term.Eta):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermEta ")
    }
    visitChild(eta)
  }
  def visitTermForYield(forYield: Term.ForYield):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermForYield ")
    }
    visitChild(forYield)
  }
  def visitTermFunction(function: Term.Function):Unit={
    functionStr=functionStr+function.toString()+"\n"+temName+"\n\n"
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermFunction ")
    }
    visitChild(function)
  }
  def visitTermIf(value: Term.If):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermIf ")
    }
    visitChild(value)
  }
  def visitLit(lit: Lit):Unit={
    lit match {
      case y:Lit.Boolean=>visitLitBoolean(y)
      case y:Lit.Byte=>visitLitByte(y)
      case y:Lit.Char=>visitLitChar(y)
      case y:Lit.Double=>visitLitDouble(y)
      case y:Lit.Float=>visitLitFloat(y)
      case y:Lit.Int=>visitLitInt(y)
      case y:Lit.Long=>visitLitLong(y)
      case y:Lit.Null=>visitLitNull(y)
      case y:Lit.Short=>visitLitShort(y)
      case y:Lit.String=>visitLitString(y)
      case y:Lit.Symbol=>visitLitSymbol(y)
      case y:Lit.Unit=>visitLitUnit(y)
      case _=>println("visitLit"+lit.toString())
    }
  }
  def visitLitBoolean(boolean: Lit.Boolean):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitBoolean ")
    }
    if((!boolean.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<boolean> ")  //replace constant value
    }
  }
  def visitLitByte(byte: Lit.Byte):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitByte ")
    }
    if((!byte.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<byte> ")  //replace constant value
    }
  }
  def visitLitChar(char: Lit.Char):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitChar ")
    }
    if((!char.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<char> ")  //replace constant value
    }
  }
  def visitLitDouble(double: Lit.Double):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitDouble ")
    }
    if((!double.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<double> ")  //replace constant value
    }
  }
  def visitLitFloat(float: Lit.Float):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitFloat ")   //replace constant value
    }
    if((!float.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<float> ")      //replace constant value
    }
  }
  def visitLitInt(int: Lit.Int):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitInt ")
    }
    if((!int.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<int> ")   //replace constant value
    }
  }
  def visitLitLong(long: Lit.Long):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitLong ")
    }
    if((!long.value.equals(""))&& begin ){
      identifierStack.push(identifierStack.pop()+"<long> ")
    }
  }
  def visitLitNull(value: Lit.Null):Unit={
    if(begin) {
      ASTStack.push(ASTStack.pop()+"LitNull ")
      identifierStack.push(identifierStack.pop()+"<null> ")   //replace constant value
    }
  }
  def visitLitShort(short: Lit.Short):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitShort ")
    }
    if((!short.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<short> ")   //replace constant value
    }
  }
  def visitLitString(str: Lit.String):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitString ")
    }
    if((!str.value.equals(""))&&begin){
      identifierStack.push(identifierStack.pop()+"<string> ")   //replace constant value
    }
  }
  def visitLitSymbol(symbol: Lit.Symbol):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitSymbol ")
    }
    if((!symbol.value.equals(""))&& begin){
      identifierStack.push(identifierStack.pop()+"<symbol> ")  //replace constant value
    }
  }
  def visitLitUnit(unit: Lit.Unit):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"LitUnit ")
      identifierStack.push(identifierStack.pop()+"<unit> ")   //replace constant value
    }
  }
  def visitTermInterpolate(interpolate: Term.Interpolate):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermInterpolate ")
    }
    visitChild(interpolate)
  }
  def visitTermMatch(value: Term.Match):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermMatch ")
    }
    visitChild(value)
  }
  def visitTermName(name: Term.Name):Unit={
    if(begin){
      identifierStack.push(identifierStack.pop()+name.value+" ")
    }
  }
  def visitTermNew(value: Term.New):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermNew ")
    }
    visitChild(value)
  }
  def visitTermNewAnonymous(anonymous: Term.NewAnonymous):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermNewAnonymous ")
    }
    visitChild(anonymous)
  }
  def visitTermParam(param:Term.Param):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermParam ")
    }
    visitChild(param)
  }
  def visitTermPartialFunction(function: Term.PartialFunction):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermPartialFunction ")
    }
    visitChild(function)
  }
  def visitTermPlaceholder(placeholder: Term.Placeholder):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermPlaceholder ")
    }
    visitChild(placeholder)
  }
  def visitTermRef(ref: Term.Ref):Unit={
    if(beginClass){
      tempPkg=ref.toString()
      beginClass=false
    }
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermRef ")
    }
    visitChild(ref)
  }
  def visitTermRepeated(repeated: Term.Repeated):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermRepeated ")
    }
    visitChild(repeated)
  }
  def visitTermReturn(value: Term.Return):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermReturn ")
    }
    visitChild(value)
  }
  def visitTermSelect(select: Term.Select):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermSelect ")
    }
    visitChild(select)
  }
  def visitTermSuper(value: Term.Super):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermSuper ")
    }
    visitChild(value)
  }
  def visitTermThis(value: Term.This):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermThis ")
    }
    visitChild(value)
  }
  def visitTermThrow(value: Term.Throw):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermThrow ")
    }
    visitChild(value)
  }
  def visitTermTryWithHandler(handler: Term.TryWithHandler):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermTryWithHandler ")
    }
    visitChild(handler)
  }
  def visitTermTuple(tuple: Term.Tuple):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermTuple ")
    }
    visitChild(tuple)
  }
  def visitTermWhile(value: Term.While):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermWhile ")
    }
    visitChild(value)
  }
  def visitTermXml(xml: Term.Xml):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermXml ")
    }
    visitChild(xml)
  }
  def visitTermFor(value: Term.For):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TermFor ")
    }
    visitChild(value)
  }
  def visitEnumerator(enumerator: Enumerator):Unit={
    enumerator match {
      case y:Enumerator.Generator=>visitEnumeratorGenerator(y)
      case y:Enumerator.Guard=>visitEnumeratorGuard(y)
      case y:Enumerator.Val=>visitEnumeratorVal(y)
      case _=>println("visitEnumerator"+enumerator.toString())
    }
  }
  def visitEnumeratorGenerator(generator: Enumerator.Generator):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"EnumeratorGenerator ")
    }
    visitChild(generator)
  }
  def visitEnumeratorGuard(guard: Enumerator.Guard):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"EnumeratorGuard ")
    }
    visitChild(guard)
  }
  def visitEnumeratorVal(value: Enumerator.Val):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"EnumeratorVal ")
    }
    visitChild(value)
  }
  def visitMod(mod: Mod):Unit={
    mod match {
      case y:Mod.Private=>visitModPrivate(y)
      case y:Mod.Abstract=>visitModAbstract(y)
      case y:Mod.Annot=>visitModAnnot(y)
      case y:Mod.Case=>visitModCase(y)
      case y:Mod.Contravariant=>visitModContravariant(y)
      case y:Mod.Covariant=>visitModCovariant(y)
      case y:Mod.Final=>visitModFinal(y)
      case y:Mod.Implicit=>visitModImplicit(y)
      case y:Mod.Inline=>visitModInline(y)
      case y:Mod.Lazy=>visitModLazy(y)
      case y:Mod.Override=>visitModOverride(y)
      case y:Mod.Protected=>visitModProtect(y)
      case y:Mod.Sealed=>visitModSealed(y)
      case y:Mod.ValParam=>visitModValParam(y)
      case y:Mod.VarParam=>visitModVarParam(y)
      case _=>println("visit mod 1"+mod.toString())
    }
  }
  def visitDefnType(value: Defn.Type):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnType ")
    }
    visitChild(value)
  }

  /**
    * Start traversing the statement inside the method body,
    * storing the value of the leaf node and the type of the intermediate statement
    * @param value
    */
  def visitDefnDef(value: Defn.Def):Unit= {
    if (!ASTStack.isEmpty) {
      ASTStack.push(ASTStack.pop() + "Defndef ")
    }
    if (value.pos.endLine-value.pos.startLine>=10) {     //filter out small statements
      begin = true
      val leafString = new String()
      val pathString = new String("DefnDef ")
      identifierStack.push(leafString)                       //solve function nesting problems
      ASTStack.push(pathString)                       //solve function nesting problems
      visitChild(value)
      sourceMethodCode.append("startLine: "+value.pos.startLine+"\nendLine: "+value.pos.endLine+"\n"+value.toString())
      var params=""
      for(strs<-value.tparams){
        params=params+strs.tbounds+" "
      }
      methodName = methodName + temName + " " + (value.pos.startLine + 1)+ " " +  (value.pos.endLine +1) +"\n"
      //methodName.append(tempPkg+"."+temClass+" "+value.name.toString()+" "+params)
      sourceFileName.append(temName)
      val tempAST = ASTStack.pop()
      ASTResult = ASTResult + tempAST + "\n"
      if(!ASTStack.isEmpty){
        ASTStack.push(ASTStack.pop()+tempAST+" ")
      }
      val tempIdentifier=identifierStack.pop()
      identifierResult = identifierResult + tempIdentifier + "\n"
      if(!identifierStack.isEmpty){
        identifierStack.push(identifierStack.pop()+tempIdentifier+" ")
      }


      begin = false
    }
  }
  def visitModCase(value: Mod.Case):Unit={
    if(begin){                                          //"begin" make sure this statement is in the method body
      ASTStack.push(ASTStack.pop()+"ModCase ")
    }
    visitChild(value)
  }
  def visitDefnVar(value: Defn.Var):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnVar ")
    }
    visitChild(value)
  }
  def visitType(value: Type):Unit={
    value match {
      case y:Type.And=>visitTypeAnd(y)
      case y:Type.Annotate=>visitTypeAnnotate(y)
      case y:Type.Apply=>visitTypeApply(y)
      case y:Type.ApplyInfix=>visitTypeApplyInfix(y)
      case y:Type.Bounds=>visitTypeBounds(y)
      case y:Type.ByName=>visitTypeByName(y)
      case y:Type.Existential=>visitTypeExistential(y)
      case y:Type.Function=>visitTypeFunction(y)
      case y: Type.ImplicitFunction=>visitTypeImplicitFunction(y)
      case y:Type.Lambda=>visitTypeLambda(y)
      case y:Type.Method=>visitTypeMethod(y)
      case y:Type.Name=>visitTypeName(y)
      case y:Type.Or=>visitTypeOr(y)
      case y:Type.Param=>visitTypeParam(y)
      case y:Type.Placeholder=>visitTypePlaceholder(y)
      case y:Type.Project=>visitTypeProject(y)
      case y:Type.Ref=>visitTypeRef(y)
      case y:Type.Refine=>visitTypeRefine(y)
      case y:Type.Repeated=>visitTypeRepeated(y)
      case y:Type.Select=>visitTypeSelect(y)
      case y:Type.Singleton=>visitTypeSingleton(y)
      case y:Type.Tuple=>visitTypeTuple(y)
      case y:Type.Var=>visitTypeVar(y)
      case y:Type.With=>visitTypeWith(y)
      case _=>println("visit Type"+value.toString())
    }
  }
  def visitTypeAnnotate(annotate: Type.Annotate):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeAnnotate ")
    }
    visitChild(annotate)
  }
  def visitTypeApplyInfix(infix: Type.ApplyInfix):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeApplyInfix ")
    }
    visitChild(infix)
  }
  def visitTypeAnd(and: Type.And):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeAnd ")
    }
    visitChild(and)
  }
  def visitTypeApply(apply: Type.Apply):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeApply ")
    }
    visitChild(apply)
  }
  def visitTypeBounds(value: Type.Bounds):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeBounds ")
    }
    visitChild(value)
  }
  def visitTypeByName(name: Type.ByName):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeByName ")
    }
    visitChild(name)
  }
  def visitTypeExistential(existential: Type.Existential):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeExistential ")
    }
    visitChild(existential)
  }
  def visitTypeFunction(function: Type.Function):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeFunction ")
    }
    visitChild(function)
  }
  def visitTypeImplicitFunction(function: Type.ImplicitFunction):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeImplicitFunction ")
    }
    visitChild(function)
  }
  def visitTypeLambda(lambda: Type.Lambda):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeLambda ")
    }
    visitChild(lambda)
  }
  def visitTypeMethod(method: Type.Method):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeMethod ")
    }
    visitChild(method)
  }
  def visitTypeName(name: Type.Name):Unit={
    if(begin){
      identifierStack.push(identifierStack.pop()+name.value+" ")
    }
  }
  def visitTypeOr(or: Type.Or):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeOr ")
    }
    visitChild(or)
  }
  def visitTypeParam(value: Type.Param):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeParam ")
    }
    visitChild(value)
  }
  def visitTypePlaceholder(placeholder: Type.Placeholder):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypePlaceholder ")
    }
    visitChild(placeholder)
  }
  def visitTypeProject(project: Type.Project):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeProject ")
    }
    visitChild(project)
  }
  def visitTypeRef(ref: Type.Ref):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeRef ")
    }
    visitChild(ref)
  }
  def visitTypeRefine(refine: Type.Refine):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeRefine ")
    }
    visitChild(refine)
  }
  def visitTypeRepeated(repeated: Type.Repeated):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeRepeated ")
    }
    visitChild(repeated)
  }
  def visitTypeSelect(select: Type.Select):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeSelect ")
    }
    visitChild(select)
  }
  def visitTypeSingleton(singleton: Type.Singleton):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeSingleton ")
    }
    visitChild(singleton)
  }
  def visitTypeTuple(tuple: Type.Tuple):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeTuple ")
    }
    visitChild(tuple)
  }
  def visitTypeVar(value: Type.Var):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeVar ")
    }
    visitChild(value)
  }
  def visitTypeWith(value: Type.With):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"TypeWith ")
    }
    visitChild(value)
  }
  def visitTemplate(template: Template):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"Template ")
    }
    visitChild(template)
  }
  def visitInit(init: Init):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"Init ")
    }
    visitChild(init)
  }
  def visitSelf(self: Self):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"Self ")
    }
    visitChild(self)
  }
  def visitDefnMacro(value: Defn.Macro):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnMacro ")
    }
    visitChild(value)
  }
  def visitDefnTrait(value: Defn.Trait):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnTrait ")
    }
    visitChild(value)
  }
  def visitDefnVal(value: Defn.Val):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnVal ")
    }
    visitChild(value)
  }
  def visitPat(pat: Pat):Unit={
    pat match {
      case y:Pat.Alternative=>visitPatAlternative(y)
      case y:Pat.Bind=>visitPatBind(y)
      case y:Pat.Extract=>visitPatExtract(y)
      case y:Pat.ExtractInfix=>visitPatExtractInfix(y)
      case y:Pat.Interpolate=>visitPatInterpolate(y)
      case y:Pat.SeqWildcard=>visitPatSeqWildcard(y)
      case y:Pat.Tuple=>visitPatTuple(y)
      case y:Pat.Typed=>visitPatTyped(y)
      case y:Pat.Var=>visitPatVar(y)
      case y:Pat.Wildcard=>visitPatWildcard(y)
      case y:Pat.Xml=>visitPatXml(y)
      case _=>println("visitPat "+pat.toString())
    }
  }
  def visitPatAlternative(alternative: Pat.Alternative):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatAlternative ")
    }
    visitChild(alternative)
  }
  def visitPatBind(bind: Pat.Bind):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatBind ")
    }
    visitChild(bind)
  }
  def visitPatExtract(extract: Pat.Extract):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatExtract ")
    }
    visitChild(extract)
  }
  def visitPatExtractInfix(infix: Pat.ExtractInfix):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatExtractInfix ")
    }
    visitChild(infix)
  }
  def visitPatInterpolate(interpolate: Pat.Interpolate):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatInterpolate ")
    }
    visitChild(interpolate)
  }
  def visitPatSeqWildcard(wildcard: Pat.SeqWildcard):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatSeqWildcard ")
    }
    visitChild(wildcard)
  }
  def visitPatTuple(tuple: Pat.Tuple):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatTuple ")
    }
    visitChild(tuple)
  }
  def visitPatTyped(typed: Pat.Typed):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatTyped ")
    }
    visitChild(typed)
  }
  def visitPatVar(value: Pat.Var):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatVar ")
    }
    visitChild(value)
  }
  def visitPatWildcard(wildcard: Pat.Wildcard):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatWildcard ")
    }
    visitChild(wildcard)
  }
  def visitPatXml(xml: Pat.Xml):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PatXml ")
    }
    visitChild(xml)
  }
  def visitModAbstract(value: Mod.Abstract):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModAbstract ")
    }
    visitChild(value)
  }
  def visitModAnnot(annot: Mod.Annot):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModAnnot ")
    }
    visitChild(annot)
  }
  def visitModContravariant(contravariant: Mod.Contravariant):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModContravariant ")
    }
    visitChild(contravariant)
  }
  def visitModCovariant(covariant: Mod.Covariant):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModCovariant ")
    }
    visitChild(covariant)
  }
  def visitModFinal(value: Mod.Final):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModFinal ")
    }
    visitChild(value)
  }
  def visitModImplicit(value: Mod.Implicit):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModImplicit ")
    }
    visitChild(value)
  }
  def visitModInline(inline: Mod.Inline):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModInline ")
    }
    visitChild(inline)
  }
  def visitModLazy(value: Mod.Lazy):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModLazy ")
    }
    visitChild(value)
  }
  def visitModOverride(value: Mod.Override):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModOverride ")
    }
    visitChild(value)
  }
  def visitModPrivate(value: Mod.Private):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModPrivate ")
    }
    visitChild(value)
  }
  def visitName(name: Name):Unit={
    name match {
      case y:Name.Anonymous=>visitNameAnonymous(y)
      case y:Name.Indeterminate=>visitNameIndeterminate(y)
      case _=>println("visitName"+name.toString())
    }
  }
  def visitNameAnonymous(anonymous: Name.Anonymous):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"NameAnonymous ")
    }
    if((!anonymous.value.equals("")) && begin){
      identifierStack.push(identifierStack.pop()+anonymous.value+" ")
    }
  }
  def visitNameIndeterminate(indeterminate: Name.Indeterminate):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"NameIndeterminate ")
    }
    if((!indeterminate.value.equals(""))&&begin){
      identifierStack.push(identifierStack.pop()+indeterminate.value+" ")
    }
  }
  def visitModProtect(value: Mod.Protected):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModProtect ")
    }
    visitChild(value)
  }
  def visitModSealed(value: Mod.Sealed):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModSealed ")
    }
    visitChild(value)
  }
  def visitModValParam(param: Mod.ValParam):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModValParam ")
    }
    visitChild(param)
  }
  def visitModVarParam(param: Mod.VarParam):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"ModVarParam ")
    }
    visitChild(param)
  }
  def visitDefnClass(clazz: Defn.Class):Unit={
    temClass=clazz.name.toString()
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnClass ")
    }
    visitChild(clazz)
  }
  def visitDefnObject(value: Defn.Object):Unit={
    temClass=value.name.toString()
    if(begin){
      ASTStack.push(ASTStack.pop()+"DefnObject ")
    }
    visitChild(value)
  }
  def visitImport(value: Import):Unit={

  }
  def visitSource(tree:Source):Unit={
    beginClass=true
    visitChild(tree)
  }
  def visitDef(defn: Defn):Unit={
    defn match {
      case y: Defn.Object=>visitDefnObject(y)
      case y:Defn.Val=>visitDefnVal(y)
      case y:Defn.Def=>visitDefnDef(y)
      case y:Defn.Class=>visitDefnClass(y)
      case y:Defn.Macro=>visitDefnMacro(y)
      case y:Defn.Trait=>visitDefnTrait(y)
      case y:Defn.Type=>visitDefnType(y)
      case y:Defn.Var=>visitDefnVar(y)
    }
  }
  def visitCase(value: Case):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"Case ")
    }
    visitChild(value)
  }
  def visitDecl(decl: Decl):Unit={
    decl match {
      case y:Decl.Def=>visitDeclDef(y)
      case y:Decl.Type=>visitDeclType(y)
      case y:Decl.Val=>visitDeclVal(y)
      case y:Decl.Var=>visitDeclVar(y)
      case _=>println("visitDecl")
    }
  }
  def visitDeclDef(value: Decl.Def):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DeclDefe ")
    }
    visitChild(value)
  }
  def visitDeclType(value: Decl.Type):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DeclType ")
    }
    visitChild(value)
  }
  def visitDeclVar(value: Decl.Var):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DeclVar ")
    }
    visitChild(value)
  }
  def visitDeclVal(value: Decl.Val):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"DeclVal ")
    }
    visitChild(value)
  }
  def visitPkg(pkg: Pkg):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"Pkg ")
    }
    visitChild(pkg)
  }
  def visitPkgObject(pkg: Pkg.Object):Unit={
    if(begin){
      ASTStack.push(ASTStack.pop()+"PkgObject ")
    }
    visitChild(pkg)
  }

  /**
    * The total recursive function,
    * and the order can not be changed
    * @param tree
    */
  def visitChild(tree:Tree):Unit={
    var nodes=tree.children
    for(a <- 0 to nodes.size-1){
      nodes(a) match {
        case y:Source=>visitSource(y)
        case y:Import=>visitImport(y)
        case y:Ctor=>visitCtor(y)
        case y:Template=>visitTemplate(y)
        case y:Init=>visitInit(y)
        case y:Self=>visitSelf(y)
        case y:Defn=>visitDef(y)
        case y:Mod=>visitMod(y)
        case y:Lit=>visitLit(y)
        case y:Enumerator=>visitEnumerator(y)
        case y:Type=>visitType(y)
        case y:Term=>visitTerm(y)
        case y:Pat=>visitPat(y)
        case y:Case=>visitCase(y)
        case y:Type.Param=>visitTypeParam(y)
        case y:Term.Param=>visitTermParam(y)
        case y:Type.Bounds=>visitTypeBounds(y)
        case y:Name=>visitName(y)
        case y:Decl=>visitDecl(y)
        case y:Pkg.Object=>visitPkgObject(y)
        case y:Pkg=>visitPkg(y)
        case _=>println("wrong of children  "+nodes(a).toString())
      }
    }
  }

  /**
    * the entrance of the AST tree
    * @param tree
    */
  def visitParent(tree:Tree):Unit={
    tree match {
      case y:Source=>visitSource(y)
      case y:Import=>visitImport(y)
      case y:Ctor=>visitCtor(y)
      case y:Template=>visitTemplate(y)
      case y:Init=>visitInit(y)
      case y:Self=>visitSelf(y)
      case y:Defn=>visitDef(y)
      case y:Mod=>visitMod(y)
      case y:Lit=>visitLit(y)
      case y:Enumerator=>visitEnumerator(y)
      case y:Type=>visitType(y)
      case y:Term=>visitTerm(y)
      case y:Pat=>visitPat(y)
      case y:Case=>visitCase(y)
      case y:Type.Param=>visitTypeParam(y)
      case y:Term.Param=>visitTermParam(y)
      case y:Type.Bounds=>visitTypeBounds(y)
      case y:Name=>visitName(y)
      case y:Decl=>visitDecl(y)
      case y:Pkg.Object=>visitPkgObject(y)
      case y:Pkg=>visitPkg(y)
      case _=>println("wrong of parent  "+tree.toString())
    }
  }

  def parse(filename: String):Unit={
    val sort=new SortFile()
    sort.sortFile(filename)
    val t=new java.io.File(filename).parse[Source].get
    visitParent(t)
  }
}
