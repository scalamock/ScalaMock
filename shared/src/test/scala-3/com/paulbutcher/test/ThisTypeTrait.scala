package com.paulbutcher.test

trait ThisTypeTrait {
  def thisTypeMethod(foo: Int): this.type
}
