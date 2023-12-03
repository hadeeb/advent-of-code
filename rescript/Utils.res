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
  external readText: URL.t => promise<t> = "readFile"
  @send
  external toString: t => string = "toString"

  let readLines = async url => {
    let file = await readText(url)
    file->toString->String.split("\n")
  }
}
