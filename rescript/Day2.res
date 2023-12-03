open Utils

type pick = Red(int) | Blue(int) | Green(int)

type game = {
  id: int,
  rounds: array<array<pick>>,
}

let parseLine = line => {
  let parts = line->String.split(":")
  let idSection = parts->Array.getUnsafe(0)
  let id =
    idSection
    ->String.sliceToEnd(~start="Game "->String.length)
    ->Float.parseInt
    ->Float.toInt

  let roundSection = parts->Array.getUnsafe(1)
  let rounds =
    roundSection
    ->String.split(";")
    ->Array.map(round => {
      round
      ->String.split(",")
      ->Array.map(pick => {
        let parts = pick->String.trim->String.split(" ")
        let count = parts->Array.getUnsafe(0)->Float.parseInt->Float.toInt
        let color = parts->Array.getUnsafe(1)->String.toLowerCase

        switch color {
        | "red" => Red(count)
        | "green" => Green(count)
        | "blue" => Blue(count)
        | _ => failwith("invalid color")
        }
      })
    })

  {id, rounds}
}

type problem = {
  red: int,
  green: int,
  blue: int,
}

let step1 = lines => {
  // 12 red cubes, 13 green cubes, and 14 blue cubes

  let problem = {
    red: 12,
    green: 13,
    blue: 14,
  }

  let matches = (problem, entry) => {
    let rounds = entry.rounds
    let fulfills = rounds->Array.every(round => {
      round->Array.every(pick => {
        switch pick {
        | Red(count) => count <= problem.red
        | Green(count) => count <= problem.green
        | Blue(count) => count <= problem.blue
        }
      })
    })
    if fulfills {
      entry.id
    } else {
      0
    }
  }

  lines->Array.reduce(0, (acc, entry) => {
    let id = matches(problem, entry)
    acc + id
  })
}

let step2 = lines => {
  let powerOfLine = line => {
    let x = line.rounds->Array.reduce(
      {
        red: 0,
        green: 0,
        blue: 0,
      },
      (acc, round) => {
        let minOfRound = round->Array.reduce(
          {
            red: 0,
            green: 0,
            blue: 0,
          },
          (acc, pick) => {
            switch pick {
            | Red(count) => {
                ...acc,
                red: Math.Int.max(acc.red, count),
              }
            | Green(count) => {
                ...acc,
                green: Math.Int.max(acc.green, count),
              }
            | Blue(count) => {
                ...acc,
                blue: Math.Int.max(acc.blue, count),
              }
            }
          },
        )
        {
          red: Math.Int.max(acc.red, minOfRound.red),
          green: Math.Int.max(acc.green, minOfRound.green),
          blue: Math.Int.max(acc.blue, minOfRound.blue),
        }
      },
    )

    x.red * x.blue * x.green
  }

  lines->Array.reduce(0, (acc, entry) => {
    acc + powerOfLine(entry)
  })
}

let run = async () => {
  let lines = await File.readLines(URL.make("../data/02.txt", ~base=Import.Meta.url))
  let lines = lines->Array.map(parseLine)

  let sum = step1(lines)
  Console.log2("Step 1: ", sum)

  let sum = step2(lines)
  Console.log2("Step 2: ", sum)
}

run()->ignore
