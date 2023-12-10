open Utils

type race = {time: int, distance: int}
let parse = async () => {
  let lines = await File.readLines(URL.make("../data/06.txt", ~base=Import.Meta.url))
  let strToNums = string => {
    string
    ->String.replaceRegExp(Re.fromStringWithFlags("\\s+", ~flags="g"), " ")
    ->String.trim
    ->String.split(" ")
    ->Array.map(x => x->Float.parseInt->Float.toInt)
  }
  let times =
    lines
    ->Array.getUnsafe(0)
    ->String.sliceToEnd(~start="Time:"->String.length)
    ->strToNums

  let distances =
    lines
    ->Array.getUnsafe(1)
    ->String.sliceToEnd(~start="Distance:"->String.length)
    ->strToNums

  assert(times->Array.length == distances->Array.length)

  Array.fromInitializer(~length=times->Array.length, index => {
    {
      time: times->Array.getUnsafe(index),
      distance: distances->Array.getUnsafe(index),
    }
  })
}

let step1 = races => {
  let waysToWin = races->Array.map(race => {
    let {time, distance: target} = race

    let timeTravelled = timeToStart => {
      let speed = timeToStart * 1
      let timeToMove = time - timeToStart
      let distanceTravelled = timeToMove * speed
      distanceTravelled
    }

    let timeToStart = ref(0)
    let wins = ref(0)
    while timeToStart.contents <= time {
      let distance = timeTravelled(timeToStart.contents)
      if distance > target {
        wins := wins.contents + 1
      }
      timeToStart := timeToStart.contents + 1
    }
    wins.contents
  })
  waysToWin->Array.reduce(1, (acc, x) => acc * x)
}

let run = async () => {
  let races = await parse()
  Console.log2("Step 1:", step1(races))
  // Console.log2("Step 2:", step2(seeds, maps))
}

run()->ignore
