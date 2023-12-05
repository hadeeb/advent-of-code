open Utils

type mapping = {
  fromStart: float,
  fromEnd: float,
  toStart: float,
  toEnd: float,
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
    let from = line->Array.getUnsafe(1)
    let to = line->Array.getUnsafe(0)
    let range = line->Array.getUnsafe(2)
    {
      fromStart: from,
      fromEnd: from +. range -. 1.0,
      toStart: to,
      toEnd: to +. range -. 1.0,
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
      acc >= mapping.fromStart && acc <= mapping.fromEnd
    })
    ->Option.map(mapping => {
      let diff = acc -. mapping.fromStart
      mapping.toStart +. diff
    })
    ->Option.getOr(acc)
  })
}

let getSeedForLocation = (location, maps) => {
  maps->Array.reduceRight(location, (acc, map) => {
    map.map
    ->Array.find(mapping => {
      acc >= mapping.toStart && acc <= mapping.toEnd
    })
    ->Option.map(mapping => {
      let diff = acc -. mapping.toStart
      mapping.fromStart +. diff
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

let getStart = (location, maps) => {
  maps->Array.reduceRight(location, (acc, map) => {
    map.map
    ->Array.find(mapping => {
      acc >= mapping.toStart && acc <= mapping.toEnd
    })
    ->Option.map(mapping => {
      let diff = acc -. mapping.toStart
      mapping.fromStart +. diff
    })
    ->Option.getOr(acc)
  })
}

type range = {
  start: float,
  end: float,
}

let step2 = (seeds, maps) => {
  // Work way up
  let smallestLocationRange = maps->Array.at(-1)->Option.getUnsafe
  let firstValue = smallestLocationRange.map->Array.getUnsafe(0)
  let smallestLocationRange =
    smallestLocationRange.map->Array.reduce(firstValue, (acc, x) =>
      acc.toStart > x.toStart ? x : acc
    )

  let start = 0.0
  let end = smallestLocationRange.toEnd

  let seedRanges = Array.fromInitializer(~length=seeds->Array.length / 2, i => {
    let start = seeds->Array.getUnsafe(i * 2)
    let range = seeds->Array.getUnsafe(i * 2 + 1)
    let end = start +. range -. 1.0
    {start, end}
  })

  let rec checkSeedExist = location => {
    let seed = getSeedForLocation(location, maps)

    let seedExists = seedRanges->Array.some(range => {
      seed >= range.start && seed <= range.end
    })
    if seedExists {
      location
    } else if location > end {
      failwith("No seed found")
    } else {
      checkSeedExist(location +. 1.0)
    }
  }
  checkSeedExist(start)
}

let run = async () => {
  let (seeds, maps) = await parse()
  Console.log2("Step 1:", step1(seeds, maps))
  Console.log2("Step 2:", step2(seeds, maps))
}

run()->ignore
