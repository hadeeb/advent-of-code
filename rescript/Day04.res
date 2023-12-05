open Utils

type game = {cards: array<int>, winners: array<int>}

let parse = async () => {
  let lines = await File.readLines(URL.make("../data/04.txt", ~base=Import.Meta.url))

  let strToArray = str => {
    str
    ->String.trim
    ->String.split(" ")
    ->Array.map(x => x->Float.parseInt->Float.toInt)
  }

  lines->Array.map(line => {
    let line = line->String.sliceToEnd(~start=9)
    let parts =
      line
      ->String.replaceRegExp(RegExp.fromStringWithFlags("\\s+", ~flags="g"), " ")
      ->String.split("|")

    let winners = parts->Array.getUnsafe(0)->strToArray
    let cards = parts->Array.getUnsafe(1)->strToArray

    {cards, winners}
  })
}

let step1 = games => {
  games->Array.reduce(0, (acc, game) => {
    let {cards, winners} = game
    let score =
      cards
      ->Array.filter(card => {
        Array.includes(winners, card)
      })
      ->Array.length
    if score == 0 {
      acc
    } else {
      acc + Math.Int.pow(2, ~exp=score - 1)
    }
  })
}

let step2 = games => {
  let scores = games->Array.map(game => {
    let {cards, winners} = game

    cards
    ->Array.filter(card => {
      Array.includes(winners, card)
    })
    ->Array.length
  })
  let cardCount = Array.make(~length=games->Array.length, 1)
  scores->Array.forEachWithIndex((score, i) => {
    let currentSetCount = cardCount->Array.getUnsafe(i)
    let start = i + 1
    let end = Math.Int.min(i + score, cardCount->Array.length - 1)
    for j in start to end {
      cardCount[j] = cardCount->Array.getUnsafe(j) + currentSetCount
    }
  })

  cardCount->Array.reduce(0, (acc, x) => acc + x)
}

let run = async () => {
  let games = await parse()

  Console.log2("Step 1:", step1(games))
  Console.log2("Step 2:", step2(games))
}

run()->ignore
