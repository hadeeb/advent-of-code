open Utils

type mapping = {
  from: float,
  to: float,
  range: float,
}

type map = {
  from: string,
  to: string,
  map: array<mapping>,
}

let parse = async () => {
  let file = await File.readFile(URL.make("../data/05.txt", ~base=Import.Meta.url))
  let file = file->File.toString
  let sections = file->String.split("\n\n")
  let header = sections->Array.shift->Option.getUnsafe
  let seeds =
    header
    ->String.sliceToEnd(~start=7)
    ->String.split(" ")
    ->Array.map(x => x->Float.parseInt)

  let line2mapping = line => {
    let line = line->String.split(" ")->Array.map(x => x->Float.parseInt)
    {
      from: line->Array.getUnsafe(1),
      to: line->Array.getUnsafe(0),
      range: line->Array.getUnsafe(2),
    }
  }

  let maps = sections->Array.map(section => {
    let lines = section->String.split("\n")
    let header = lines->Array.shift->Option.getUnsafe
    let header = header->String.split(" ")->Array.getUnsafe(0)->String.split("-")
    {
      from: header->Array.getUnsafe(0),
      to: header->Array.getUnsafe(2),
      map: lines->Array.map(line2mapping),
    }
  })

  (seeds, maps)
}

let getLocationForSeed = (seed, maps) => {
  maps->Array.reduce(seed, (acc, map) => {
    map.map
    ->Array.find(mapping => {
      let start = mapping.from
      let end = mapping.from +. mapping.range -. 1.0
      acc >= start && acc <= end
    })
    ->Option.map(mapping => {
      let diff = acc -. mapping.from
      mapping.to +. diff
    })
    ->Option.getOr(acc)
  })
}

let step1 = (seeds, maps) => {
  seeds
  ->Array.map(seed => {
    getLocationForSeed(seed, maps)
  })
  ->Array.reduce(Float.Constants.positiveInfinity, (acc, x) => acc > x ? x : acc)
}

let step2 = (seeds: array<float>, maps) => {
  // Does not work. Loop too large.
  let smallest = ref(Float.Constants.positiveInfinity)

  for i in 0 to seeds->Array.length - 1 {
    if i->mod(2) == 0 {
      let start = seeds->Array.getUnsafe(i)
      let range = seeds->Array.getUnsafe(i + 1)
      let end = start +. range -. 1.0
      let start = ref(start)

      while start.contents <= end {
        let location = getLocationForSeed(start.contents, maps)
        if location < smallest.contents {
          smallest := location
        }
        start := start.contents +. 1.0
      }
    }
  }

  smallest.contents
}

let run = async () => {
  let (seeds, maps) = await parse()
  Console.log2("Step 1:", step1(seeds, maps))
  // 157211394
  Console.log2("Step 2:", step2(seeds, maps))
}

run()->ignore
