package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {

    // Find the root directory
    val root = state.root
    val wd = state.wd

    // Find the absolute path of the dir to cd to
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // Find the dir to cd to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)

    // Change the state, given the new directory
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory")
    else
      State(root, destinationDirectory.asDirectory)

  }

  def doFindEntry(root: Directory, path: String): DirEntry = {

    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry =
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }

    // Get tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // Navigate to the correct entry
    findEntryHelper(root, tokens)

  }
}
