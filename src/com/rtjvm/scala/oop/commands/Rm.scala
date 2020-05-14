package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.filesystem.State
import com.rtjvm.scala.oop.files.Directory

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    // Get working dir
    val wd = state.wd

    // Get absolute path
    val absolutePath =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // Do some checks
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Sorry, this command is not supported.")
    else
      doRm(state, absolutePath)
  }

  def doRm(state: State, path: String): State = {

    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDirectory = currentDirectory.findEntry(path.head)
        if (!nextDirectory.isDirectory) currentDirectory
        else {
          val newNextDirectory = rmHelper(nextDirectory.asDirectory, path.tail)
          if (newNextDirectory == nextDirectory) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDirectory)
        }
      }
    }

    // Find the entry to remove
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root)
      state.setMessage(path + ": no such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))

    // Update structure like in mkdir

  }
}
