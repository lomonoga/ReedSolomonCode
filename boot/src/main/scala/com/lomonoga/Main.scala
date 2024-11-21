package com.lomonoga

object Main {
  def main(args: Array[String]): Unit = {
    val data = Array(1, 2, 3, 4)
    val n = 255
    val k = 223

    // Генерация генераторного полинома
    val generator = generateGeneratorPolynomial(n, k)

    // Кодирование
    val encoded = encode(data, generator)
    println(s"Кодированное сообщение: ${encoded.mkString(", ")}")

    // Симуляция ошибок
    encoded(10) ^= 5 // Добавляем ошибку
    encoded(20) ^= 7 // Еще одна ошибка

    // Декодирование
    val syndromes = computeSyndromes(encoded, n, k)
    val corrected = correctErrors(encoded, syndromes)
    println(s"Исправленное сообщение: ${corrected.mkString(", ")}")

  }

  def generateGeneratorPolynomial(n: Int, k: Int): Array[Int] = {
    var g = Array(1) // Начинаем с G(x) = 1
    for (i <- 0 until (n - k)) {
      g.indices.reverse.foreach { j =>
        g(j) = GF256.multiply(g(j), GF256.exp(i)) ^
          (if (j > 0) g(j - 1) else 0)
      }
      g :+= 0
    }
    g
  }

  def encode(data: Array[Int], g: Array[Int]): Array[Int] = {
    val n = data.length + g.length - 1
    val codeword = Array.ofDim[Int](n)

    // Копируем данные в начало кодового слова
    Array.copy(data, 0, codeword, 0, data.length)

    for (i <- data.indices) {
      val coeff = codeword(i)
      if (coeff != 0) {
        for (j <- g.indices) {
          codeword(i + j) ^= GF256.multiply(coeff, g(j))
        }
      }
    }

    codeword
  }

  def computeSyndromes(received: Array[Int], n: Int, k: Int): Array[Int] = {
    val syndromes = Array.ofDim[Int](n - k)
    for (i <- syndromes.indices) {
      syndromes(i) = received.indices.foldLeft(0) { (sum, j) =>
        GF256.add(sum, GF256.multiply(received(j), GF256.exp(i * j)))
      }
    }
    syndromes
  }

  def correctErrors(received: Array[Int], syndromes: Array[Int]): Array[Int] = {
    val errorLocator = Array(1)
    var oldLocator = Array(1)

    for (i <- syndromes.indices) {
      var delta = syndromes(i)
      for (j <- 1 until errorLocator.length) {
        delta ^= GF256.multiply(errorLocator(j), syndromes(i - j))
      }

      if (delta != 0) {
        val newLocator =
          oldLocator.map(x => GF256.multiply(x, delta)) ++ Array(0)
        for (j <- errorLocator.indices) {
          newLocator(j) ^= errorLocator(j)
        }

        if (2 * errorLocator.length <= i) {
          oldLocator.indices.foreach(j =>
            oldLocator(j) = GF256.divide(oldLocator(j), delta)
          )
          oldLocator :+= 0
        }
      }
    }
    errorLocator
  }
}
