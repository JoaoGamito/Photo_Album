import Manipulation.Coords

object Mirror {

  def mirrorV(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorV(tree, Utils.getRootCoords(tree))
  }

  def subMirrorV(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QNode(c,fi,se,th,fo) => QNode(coordsTemp,
        subMirrorV(th, Utils.trueQuad1(coordsTemp)),
        subMirrorV(fo, Utils.trueQuad2(coordsTemp)),
        subMirrorV(fi, Utils.trueQuad3(coordsTemp)),
        subMirrorV(se, Utils.trueQuad4(coordsTemp)))
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))

    }
  }

  def mirrorH(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorH(tree, Utils.getRootCoords(tree))
  }

  def subMirrorH(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QNode(c, fi, se, th, fo) => QNode(coordsTemp,
        subMirrorH(se, Utils.trueQuad1(coordsTemp)),
        subMirrorH(fi, Utils.trueQuad2(coordsTemp)),
        subMirrorH(fo, Utils.trueQuad3(coordsTemp)),
        subMirrorH(th, Utils.trueQuad4(coordsTemp)))
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))
    }
  }



}
