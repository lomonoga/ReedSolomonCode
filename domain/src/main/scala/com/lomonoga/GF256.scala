package com.lomonoga

object GF256 {
  private val fieldSize: Int = 256
  private val primitivePolynomial: Int = 0x11d

  private val expTable = Array.ofDim[Int](fieldSize)
  private val logTable = Array.ofDim[Int](fieldSize)

  // Инициализация таблиц экспонент и логарифмов
  private def initialize(): Unit = {
    var x = 1
    for (i <- 0 until fieldSize - 1) {
      expTable(i) = x
      logTable(x) = i
      x <<= 1
      if (x >= fieldSize) x ^= primitivePolynomial
    }
  }

  initialize()

  def add(a: Int, b: Int): Int = subtract(a, b)
  def subtract(a: Int, b: Int): Int = a ^ b

  def multiply(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) 0
    else expTable((logTable(a) + logTable(b)) % (fieldSize - 1))
  }

  def divide(a: Int, b: Int): Int = {
    require(b != 0, "Деление на 0 невозможно!")
    if (a == 0) 0
    else
      expTable((logTable(a) - logTable(b) + (fieldSize - 1)) % (fieldSize - 1))
  }

  def exp(a: Int): Int = expTable(
    (a % (fieldSize - 1) + (fieldSize - 1)) % (fieldSize - 1)
  )

  def log(a: Int): Int = {
    require(a != 0, "Логарифм 0 не определен в поле GF(256)!")
    logTable(a)
  }
}
