package scala.publish.dir

import scala.publish.Content
import scala.publish.fileset.SideFiles

final case class DirContent(elements: Seq[(String, Content)]) {

  /** Adds the files of `other`, removing from this [[DirContent]] the files it overrides, along
    * with their signatures and checksums
    */
  def ++(other: DirContent): DirContent = {
    val overridden = other.elements.iterator.map(_._1).toSet
    val kept       = elements.filter {
      case (n, _) =>
        !SideFiles.isSameOrSideFileOfAny(n, overridden)
    }
    DirContent(kept ++ other.elements)
  }
  def filterOutExtension(extension: String): DirContent = {
    val suffix = "." + extension
    DirContent(elements.filter(!_._1.endsWith(suffix)))
  }
  def isEmpty: Boolean =
    elements.isEmpty

  /** Removes `name`, along with its signatures and checksums */
  def remove(name: String): DirContent =
    copy(
      elements = elements.filter {
        case (n, _) => !SideFiles.isSameOrSideFileOf(n, name)
      }
    )

  /** Removes anything looking like a checksum or signature related to `path` */
  def removeAll(name: String): DirContent = {

    val (remove, keep) = elements.partition {
      case (n, _) =>
        SideFiles.isSameOrSideFileOf(n, name)
    }

    if remove.isEmpty then this else DirContent(keep)
  }

  def update(name: String, content: Content): DirContent =
    ++(DirContent(Seq(name -> content)))
}

object DirContent {
  val empty: DirContent = DirContent(Nil)
}
