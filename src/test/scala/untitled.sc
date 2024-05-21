case class Apple(weightInGram: Int, color: String)
case class Orange(weightInGram: Int)

def function(orange: Orange): Unit = {
  println(orange)
}

implicit def AppleCanBeUsedAsOrange(apple: Apple): Orange =
  Orange(apple.weightInGram)

function(
  Apple(
    weightInGram = 300,
    color = "red"
  )
)