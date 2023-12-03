open Utils

let intAtString = (str, index) => {
  let code = str->String.charCodeAt(index)->Float.toInt
  let zero = '0'->Char.code
  let nine = '9'->Char.code
  if code >= zero && code <= nine {
    Some(code - zero)
  } else {
    None
  }
}

let step1 = lines => {
  let calcLine = line => {
    let length = line->String.length

    let rec loop = (index, increment) => {
      if index >= 0 && index < length {
        switch line->intAtString(index) {
        | Some(x) => x
        | None => loop(index + increment, increment)
        }
      } else {
        0
      }
    }

    let first = loop(0, 1)
    let last = loop(length - 1, -1)
    first * 10 + last
  }

  lines->Array.reduce(0, (acc, line) => acc + calcLine(line))
}

let step2 = lines => {
  let numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

  let lineStartsWith = (line, offset) => {
    switch numbers->Array.findIndex(entry => {
      String.startsWithFrom(line, entry, offset)
    }) {
    | -1 => None
    | x => Some(x + 1)
    }
  }

  let lineEndsWith = (line, offset) => {
    switch numbers->Array.findIndex(entry => {
      String.endsWithFrom(line, entry, offset)
    }) {
    | -1 => None
    | x => Some(x + 1)
    }
  }

  let calcLine = line => {
    let max = line->String.length
    let min = 0

    let rec getFirstNumber = index => {
      if index < max {
        let x = switch line->intAtString(index) {
        | None => lineStartsWith(line, index)
        | z => z
        }
        switch x {
        | None => getFirstNumber(index + 1)
        | Some(x) => x
        }
      } else {
        0
      }
    }

    let rec getLastNumber = index => {
      if index > min {
        let x = switch line->intAtString(index - 1) {
        | None => lineEndsWith(line, index)
        | z => z
        }
        switch x {
        | None => getLastNumber(index - 1)
        | Some(x) => x
        }
      } else {
        0
      }
    }

    let first = getFirstNumber(0)
    let last = getLastNumber(max)
    first * 10 + last
  }

  lines->Array.reduce(0, (acc, line) => acc + calcLine(line))
}

let run = async () => {
  let lines = await File.readLines(URL.make("../data/01.txt", ~base=Import.Meta.url))

  let sum = step1(lines)
  Console.log2("Step 1: ", sum)

  let sum = step2(lines)
  Console.log2("Step 2: ", sum)
}

run()->ignore
