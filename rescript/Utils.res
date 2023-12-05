module Import = {
  module Meta = {
    external url: string = "import.meta.url"
  }
}
module URL = {
  type t
  @new external make: (string, ~base: string=?) => t = "URL"
}

module File = {
  type t

  @module("node:fs/promises")
  external readFile: URL.t => promise<t> = "readFile"
  @send
  external toString: t => string = "toString"

  let readString = async url => {
    let file = await readFile(url)
    file->toString
  }

  let readLines = async url => {
    let str = await readString(url)
    str->String.split("\n")
  }
}
