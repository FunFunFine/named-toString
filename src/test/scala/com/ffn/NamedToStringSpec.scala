package com.ffn

import com.ffn.NamedToString.hidden

class NamedToStringSpec extends munit.FunSuite {

  test("generate toString for simple case class with two fields") {
    val foo = Foo(3, "abc")
    assertEquals(foo.toString, "Foo(bar=3, baz=abc)")
  }

  test("generate arbitrary method for simple case class with two fields") {
    case class Bar(a: Int, b: Int) {
      def toLog: String = NamedToString.forThisClass
    }
    val bar = Bar(3, 4)
    assertEquals(bar.toLog, "Bar(a=3, b=4)")
    assertEquals(bar.toString, "Bar(3,4)")
  }

  test("generate toString for simple case class with two fields when one is hidden") {
    val foo = FooHidden(3, "abc")
    assertEquals(foo.toString, "FooHidden(bar=3)")
  }

  test("generate toString for case class with multiple fields") {
    val person = Person("Alice", 30, "alice@example.com")
    assertEquals(person.toString, "Person(name=Alice, age=30, email=alice@example.com)")
  }

  test("generate toString for empty case class") {
    val empty = Empty()
    assertEquals(empty.toString, "Empty()")
  }

  test("handle different parameter values correctly") {
    val foo1 = Foo(0, "")
    assertEquals(foo1.toString, "Foo(bar=0, baz=)")

    val foo2 = Foo(-42, "test with spaces")
    assertEquals(foo2.toString, "Foo(bar=-42, baz=test with spaces)")
  }

  test("handle null values") {
    val fooWithNull = Foo(5, null)
    assertEquals(fooWithNull.toString, "Foo(bar=5, baz=null)")
  }

  test("work with single parameter case class") {
    case class Single(value: Int) {
      override def toString: String = NamedToString.forThisClass
    }

    val single = Single(42)
    assertEquals(single.toString, "Single(value=42)")
  }

  test("work with different data types") {
    case class Mixed(
      intVal: Int,
      stringVal: String,
      doubleVal: Double,
      boolVal: Boolean,
      listVal: List[Int]
    ) {
      override def toString: String = NamedToString.forThisClass
    }

    val mixed = Mixed(1, "test", 3.14, true, List(1, 2, 3))
    assertEquals(mixed.toString, "Mixed(intVal=1, stringVal=test, doubleVal=3.14, boolVal=true, listVal=List(1, 2, 3))")
  }

  test("work with Option types") {
    case class WithOption(id: Int, maybeValue: Option[String]) {
      override def toString: String = NamedToString.forThisClass
    }

    val withSome = WithOption(1, Some("present"))
    assertEquals(withSome.toString, "WithOption(id=1, maybeValue=Some(present))")

    val withNone = WithOption(2, None)
    assertEquals(withNone.toString, "WithOption(id=2, maybeValue=None)")
  }

  test("handle special characters in strings") {
    val fooSpecial = Foo(1, "hello\"world\n")
    assertEquals(fooSpecial.toString, "Foo(bar=1, baz=hello\"world\n)")
  }

  test("work with nested case classes") {
    case class Inner(x: Int) {
      override def toString: String = NamedToString.forThisClass
    }

    case class Outer(name: String, inner: Inner) {
      override def toString: String = NamedToString.forThisClass
    }

    val outer = Outer("test", Inner(42))
    assertEquals(outer.toString, "Outer(name=test, inner=Inner(x=42))")
  }

  test("do the injection thing?") {
    val foo1 = Foo(0, "hop, in=ject")
    assertEquals(foo1.toString, "Foo(bar=0, baz=hop, in=ject)")
  }

  case class Foo(bar: Int, baz: String) {
    override def toString: String = NamedToString.forThisClass
  }

  case class FooHidden(bar: Int, @hidden() baz: String) {
    override def toString: String = NamedToString.forThisClass
  }

  case class Person(name: String, age: Int, email: String) {
    override def toString: String = NamedToString.forThisClass
  }

  case class Empty() {
    override def toString: String = NamedToString.forThisClass
  }

}
