open Utils

type race = {time: float, distance: float}
let parse = async () => {
  let lines = await File.readLines(URL.make("../data/06.txt", ~base=Import.Meta.url))
  let strToNums = string => {
    string
    ->String.replaceRegExp(Re.fromStringWithFlags("\\s+", ~flags="g"), " ")
    ->String.trim
    ->String.split(" ")
    ->Array.map(x => x->Float.parseInt)
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

let findWaysToWin = race => {
  let {time, distance: target} = race

  let timeTravelled = timeToStart => {
    let speed = timeToStart *. 1.0
    let timeToMove = time -. timeToStart
    let distanceTravelled = timeToMove *. speed
    distanceTravelled
  }

  let timeToStart = ref(0.0)
  let wins = ref(0)
  while timeToStart.contents <= time {
    let distance = timeTravelled(timeToStart.contents)
    if distance > target {
      wins := wins.contents + 1
    }
    timeToStart := timeToStart.contents +. 1.0
  }
  wins.contents
}

let step1 = races => {
  let waysToWin = races->Array.map(findWaysToWin)
  waysToWin->Array.reduce(1, (acc, x) => acc * x)
}

let parse2 = async () => {
  let lines = await File.readLines(URL.make("../data/06.txt", ~base=Import.Meta.url))
  let strToNum = string => {
    string
    ->String.replaceRegExp(Re.fromStringWithFlags("\\s*", ~flags="g"), "")
    ->String.trim
    ->Float.parseInt
  }
  let time =
    lines
    ->Array.getUnsafe(0)
    ->String.sliceToEnd(~start="Time:"->String.length)
    ->strToNum
  let distance =
    lines
    ->Array.getUnsafe(1)
    ->String.sliceToEnd(~start="Distance:"->String.length)
    ->strToNum

  {time, distance}
}

let step2 = race => findWaysToWin(race)

let run = async () => {
  Console.log2("Step 1:", step1(await parse()))
  Console.log2("Step 2:", step2(await parse2()))
}

run()->ignore
