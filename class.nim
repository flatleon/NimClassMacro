{.experimental.}

import typetraits
import macros
import strutils

type TraverseOp = enum Continue, Break, SkipChild, SkipSibling, Finished
proc traverse(n: NimNode; action:proc(n:NimNode;parents:ref seq[NimNode]):TraverseOp; parents:ref seq[NimNode] = nil ):TraverseOp {.discardable.} =
    var parents = parents
    if parents == nil:
        parents.new
        parents.newseq( 0 )
    parents.add( n )
    defer:
        discard parents.pop()

    for it in n.children:
        case action( it, parents )
        of Continue: discard
        of Break: return Break
        of SkipSibling: return SkipSibling
        of SkipChild: continue
        else: assert false

        case traverse( it, action, parents )
        of Break: return Break
        else: discard
    return Finished

proc findNode(n:NimNode,kind:NimNodeKind):NimNode=
    var ret: NimNode
    n.traverse do (n:NimNode;parents:ref seq[NimNode]) -> TraverseOp:
        if n.kind == kind:
            ret = n
            return Break
    result = ret

proc convertToMemberProc( procDefNode:NimNode, className:string ) =
    ## プロシージャをメンバ関数化する -> 第一引数に指定の型のselfパラメータを追加するだけ

    var formalParamsNode = procDefNode[3]
    formalParamsNode.expectKind( nnkFormalParams )
    var selfIdentNode = newIdentDefs( ident("self"), ident(className) )
    formalParamsNode.insert( 1, selfIdentNode )

macro classproc*(className:untyped,stmtList:untyped):untyped=
    var classNameStr = className.strVal
    stmtList.traverse do (n:NimNode;parents:ref seq[NimNode]) -> TraverseOp:
        case n.kind
        of nnkProcDef, nnkMethodDef, nnkIteratorDef:
            n.convertToMemberProc( classNameStr )
        else: discard
    result = stmtList

proc newClassDef(classNameIdent,baseNameIdent,classBody:NimNode):NimNode=
    ## クラス名ノード、基底クラス名ノード、フィールド定義ノードからクラス定義ノード(実際はtypeセクション)を作成する

    # [1].とりあえずフィールド無しのobject型定義ノードを作成
    # [2].classBodyを走査しidentDefsを見つけ次第、[1]内のRecListへコピーしていく
    #     また、ProcDefを見つけた場合は、第一引数にselfを追加してから、resultノードへProcDefを追加する

    if classBody == nil: return

    # [1]
    var classStmt = quote:
        type `classNameIdent` = ref object of `baseNameIdent`
    result = newStmtList( classStmt )

    # RecListノードを取得(なければ作る)
    var objectTyNode = classStmt.findNode( nnkObjectTy )
    if objectTyNode[2].kind == nnkEmpty:
        objectTyNode.del( 2 )
        objectTyNode.add( newNimNode( nnkRecList ) )
    var recListNode = objectTyNode[2]

    # [2]
    var result2 = result
    classBody.traverse do (n:NimNode;parents:ref seq[NimNode]) -> TraverseOp:
        case n.kind
        # プロシージャの引数にselfを追加
        of nnkProcDef, nnkMethodDef, nnkIteratorDef:
            var newNode = n.copyNimTree()
            result2.add( newNode )
            newNode.convertToMemberProc( classNameIdent.strVal )
            return SkipChild
        # 変数定義はフィールド定義へ追加する
        of nnkIdentDefs:
            recListNode.add( n )
            return SkipChild
        # 型定義は型名をクラス名+型名に変更する
        of nnkTypeSection:
            # TypeSection内の識別子にクラス名を挿入
            n.traverse do (n:NimNode;parents:ref seq[NimNode]) -> TraverseOp:
                case n.kind
                of nnkTypeDef:
                    var parentNode = parents[^1]
                    if parentNode.kind == nnkTypeSection:
                        # クラス名.型名でアクセスできるようにするためのヘルパーtemplateを定義
                        var helperTemplateStr = "template $1(T:typedesc[$2]) : untyped = `T $1`".format( n[0].strVal, classNameIdent.strVal )
                        result2.add( parseStmt( helperTemplateStr ) )
                        # 「型名」を「型名+クラス名」に変更
                        n[0] = newIdentNode( classNameIdent.strVal & n[0].strVal )
                    return SkipChild
                else:discard
            result2.add( n.copyNimTree() ) # TypeSectionまるごとコピー
            return SkipChild
        else: discard

macro class*(className:untyped,classBody:untyped):untyped=
    # クラス名と基底クラス名を取得
    var classNameStr:string
    var baseNameStr:string
    case className.len()
    of 0: # class a
        classNameStr = className.strVal
    of 2: # class a(b)
        classNameStr = className[0].strVal
        baseNameStr = className[1].strVal
    of 3: # class a of b
        classNameStr = className[1].strVal
        baseNameStr = className[2].strVal
    else: assert false
    result = newClassDef( newIdentNode(classNameStr), if baseNameStr.len() > 0:newIdentNode(baseNameStr) else:newIdentNode("RootObj"),classBody)
