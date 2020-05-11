package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists.")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separators.")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name")
    } else {
      doMkdir(state, name)
    }

  }

  def checkIllegal(str: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }

    }

    val wd = state.wd

    // Get all directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath

    // Update the new structure with new dir entry in wd
    val newDirectory = Directory.empty(wd.path, name)

    // Update the whole directory structure starting with the root
    val newRoot = updateStructure(state.root, allDirsInPath, newDirectory)

    // After new dir structure established, find new wd INSTANCE given wd's full path in new directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)

  }
}
