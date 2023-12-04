open Utils
@unboxed
type entry = Num(int) | Empty | Gear | Symbol

let parse = async () => {
  let lines = await File.readLines(URL.make("../data/03.txt", ~base=Import.Meta.url))
  let map = Map.make()
  let id = ref(0)

  let isNumber = code => {
    let zero = '0'->Char.code
    let nine = '9'->Char.code
    code >= zero && code <= nine
  }

  let codeToNumber = code => {
    let zero = '0'->Char.code
    code - zero
  }

  let rec getNumber = (line, index, acc) => {
    let max = line->String.length
    if index >= max {
      (acc, index)
    } else {
      let code = line->String.charCodeAt(index + 1)->Float.toInt
      if isNumber(code) {
        getNumber(line, index + 1, acc * 10 + codeToNumber(code))
      } else {
        (acc, index)
      }
    }
  }

  let tranformed = lines->Array.map(line => {
    let index = ref(0)
    let max = line->String.length

    let transformedLine = Array.make(~length=max, Empty)

    while index.contents < max {
      let code = line->String.charCodeAt(index.contents)->Float.toInt
      if isNumber(code) {
        let start = ref(index.contents)
        let (num, end) = getNumber(line, index.contents, codeToNumber(code))
        index := end
        let thisId = id.contents
        map->Map.set(thisId, num)
        id := id.contents + 1
        while start.contents <= end {
          transformedLine[start.contents] = Num(thisId)
          start := start.contents + 1
        }
      } else if code == '.'->Char.code {
        transformedLine[index.contents] = Empty
      } else if code == '*'->Char.code {
        transformedLine[index.contents] = Gear
      } else {
        transformedLine[index.contents] = Symbol
      }
      index := index.contents + 1
    }
    transformedLine
  })

  (map, tranformed)
}

let step1 = (map, matrix) => {
  let pickValids = (line, previousLine, nextLine) => {
    let valids = []
    let index = ref(0)
    let max = line->Array.length

    let getNeighbours = (start, end) => {
      let neighbours = Array.concat(
        previousLine->Array.slice(~start, ~end=end + 1),
        nextLine->Array.slice(~start, ~end=end + 1),
      )
      neighbours->Array.push(line[start]->Option.getOr(Empty))
      neighbours->Array.push(line[end]->Option.getOr(Empty))
      neighbours
    }

    while index.contents < max {
      let current = line[index.contents]->Option.getOr(Empty)
      switch current {
      | Num(n) => {
          let start = Math.Int.max(index.contents - 1, 0)
          while (
            index.contents + 1 < max && line[index.contents + 1]->Option.getOr(Empty) == Num(n)
          ) {
            index := index.contents + 1
          }
          let end = Math.Int.min(index.contents + 1, max - 1)

          let neighbours = getNeighbours(start, end)

          let isValid = neighbours->Array.some(item => item === Symbol || item === Gear)

          if isValid {
            valids->Array.push(map->Map.get(n)->Option.getOr(0))
          }
        }
      | _ => ()
      }
      index := index.contents + 1
    }
    valids
  }
  let nums =
    matrix
    ->Array.mapWithIndex((line, index) => {
      let previousLine = matrix[index - 1]->Option.getOr([])
      let nextLine = matrix[index + 1]->Option.getOr([])
      pickValids(line, previousLine, nextLine)
    })
    ->Array.flat
  nums->Array.reduce(0, (acc, n) => acc + n)
}

let step2 = (map, matrix) => {
  let pickValids = (line, previousLine, nextLine) => {
    let valids = []
    let index = ref(0)
    let max = line->Array.length

    let getNeighbours = (start, end) => {
      let neighbours = Array.concat(
        previousLine->Array.slice(~start, ~end=end + 1),
        nextLine->Array.slice(~start, ~end=end + 1),
      )
      neighbours->Array.push(line[start]->Option.getOr(Empty))
      neighbours->Array.push(line[end]->Option.getOr(Empty))
      neighbours
    }

    while index.contents < max {
      let current = line[index.contents]->Option.getOr(Empty)
      switch current {
      | Gear => {
          let start = Math.Int.max(index.contents - 1, 0)
          let end = Math.Int.min(index.contents + 1, max - 1)

          let neighbours = getNeighbours(start, end)

          let numsOnly =
            neighbours
            ->Array.map(item =>
              switch item {
              | Num(x) => map->Map.get(x)->Option.getOr(0)
              | _ => 0
              }
            )
            ->Array.filter(x => x != 0)
            ->Set.fromArray

          if numsOnly->Set.size == 2 {
            let gearRatio =
              numsOnly->Set.values->Core__Iterator.toArray->Array.reduce(1, (acc, n) => acc * n)
            valids->Array.push(gearRatio)
          }
        }
      | _ => ()
      }
      index := index.contents + 1
    }
    valids
  }
  let nums =
    matrix
    ->Array.mapWithIndex((line, index) => {
      let previousLine = matrix[index - 1]->Option.getOr([])
      let nextLine = matrix[index + 1]->Option.getOr([])
      line->pickValids(previousLine, nextLine)
    })
    ->Array.flat
  nums->Array.reduce(0, (acc, n) => acc + n)
}

let run = async () => {
  let (map, newMatrix) = await parse()
  Console.log2("Step 1:", step1(map, newMatrix))
  Console.log2("Step 2:", step2(map, newMatrix))
}

run()->ignore
