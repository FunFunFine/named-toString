package com.ffn
import com.ffn.NamedToString.hidden

class withNamedToStringSpec extends munit.FunSuite {

  test("generate toString for simple case class") {
    val foo = Foo(3, "abc")
    assertEquals(foo.toString, "Foo(bar=3, baz=abc)")
  }

  test("generate toString for case class with hidden fields") {
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

  test("generate toString for single parameter case class") {
    val single = Single(42)
    assertEquals(single.toString, "Single(value=42)")
  }

  test("work with case classes that have companion objects") {
    val obj1 = WithCompanion(1, "test")
    assertEquals(obj1.toString, "WithCompanion(x=1, y=test)")

    val obj2 = WithCompanion.create(5)
    assertEquals(obj2.toString, "WithCompanion(x=5, y=default)")
  }

  test("preserve companion object methods") {
    @withNamedToString
    case class MyClass(value: Int)

    object MyClass {
      def fromString(s: String): MyClass = MyClass(s.toInt)
      val default: MyClass = MyClass(0)
    }

    assertEquals(MyClass.fromString("42").toString, "MyClass(value=42)")
    assertEquals(MyClass.default.toString, "MyClass(value=0)")
  }

  test("preserve all class members") {
    @withNamedToString
    case class WithMembers(x: Int, y: String) {
      def double: Int = x * 2
      val computed: String = y.toUpperCase
      lazy val lazyValue: Int = x + 1
    }

    val obj = WithMembers(5, "test")
    assertEquals(obj.toString, "WithMembers(x=5, y=test)")
    assertEquals(obj.double, 10)
    assertEquals(obj.computed, "TEST")
    assertEquals(obj.lazyValue, 6)
  }

  test("preserve case class functionality") {
    @withNamedToString
    case class Product(id: Int, name: String)

    val p1 = Product(1, "item")
    val p2 = Product(1, "item")
    val p3 = Product(2, "other")

    assertEquals(p1.copy(name = "updated").toString, "Product(id=1, name=updated)")
    assertEquals(p1, p2)
    assertNotEquals(p1, p3)
    assertEquals(p1.hashCode, p2.hashCode)

    p1 match {
      case Product(id, name) =>
        assertEquals(id, 1)
        assertEquals(name, "item")
    }
  }

  test("work with private case classes") {
    @withNamedToString
    case class PrivateClass(value: Int)

    val obj = PrivateClass(42)
    assertEquals(obj.toString, "PrivateClass(value=42)")
  }

  test("work with sealed case classes") {
    sealed trait Base

    @withNamedToString
    case class Derived1(x: Int) extends Base

    @withNamedToString
    case class Derived2(y: String) extends Base

    val d1: Base = Derived1(10)
    val d2: Base = Derived2("test")

    assertEquals(d1.toString, "Derived1(x=10)")
    assertEquals(d2.toString, "Derived2(y=test)")
  }

  test("preserve type parameters") {
    @withNamedToString
    case class Generic[T](value: T)

    val intGen = Generic(42)
    assertEquals(intGen.toString, "Generic(value=42)")

    val stringGen = Generic("test")
    assertEquals(stringGen.toString, "Generic(value=test)")

    val listGen = Generic(List(1, 2, 3))
    assertEquals(listGen.toString, "Generic(value=List(1, 2, 3))")
  }

  test("preserve multiple type parameters") {
    @withNamedToString
    case class Pair[A, B](first: A, second: B)

    val pair = Pair(1, "one")
    assertEquals(pair.toString, "Pair(first=1, second=one)")
  }

  test("work with companion object containing apply method") {
    @withNamedToString
    case class Custom(x: Int, y: Int)

    object Custom {
      def apply(single: Int): Custom = Custom(single, single)
    }

    val c1 = Custom(1, 2)
    assertEquals(c1.toString, "Custom(x=1, y=2)")

    val c2 = Custom(5)
    assertEquals(c2.toString, "Custom(x=5, y=5)")
  }

  test("preserve companion object with values and types") {
    @withNamedToString
    case class Config(host: String, port: Int)

    object Config {
      val defaultHost = "localhost"
      val defaultPort = 8080
      type ConfigMap = Map[String, Config]

      def default: Config = Config(defaultHost, defaultPort)
    }

    assertEquals(Config.defaultHost.toString, "localhost")
    assertEquals(Config.defaultPort, 8080)
    assertEquals(Config.default.toString, "Config(host=localhost, port=8080)")

    val configs: Config.ConfigMap = Map("prod" -> Config("prod.example.com", 443))
    assertEquals(configs("prod").toString, "Config(host=prod.example.com, port=443)")
  }

  test("only work on case classes") {
    assertNoDiff(
      compileErrors("""
      @withNamedToString
      class NotACaseClass(x: Int)
    """),
      """|error: @withNamedToString can only be applied to case classes
       |      @withNamedToString
       |       ^
       |""".stripMargin
    )
  }

  test("reject traits") {
    assertNoDiff(
      compileErrors("""
      @withNamedToString
      trait SomeTrait
    """),
      """|error: @withNamedToString can only be applied to case classes
       |      @withNamedToString
       |       ^
       |""".stripMargin
    )
  }

  test("reject objects") {
    assertNoDiff(
      compileErrors("""
      @withNamedToString
      object SomeObject
    """),
      """|error: @withNamedToString can only be applied to case classes
       |      @withNamedToString
       |       ^
       |""".stripMargin
    )
  }

  test("reject case classes that already define toString") {
    assertNoDiff(
      compileErrors("""
      @withNamedToString
      case class AlreadyHasToString(x: Int) {
        override def toString = "custom"
      }
    """),
      """|error: Case class AlreadyHasToString already defines toString. Remove the @withNamedToString annotation or the toString method.
       |      @withNamedToString
       |       ^
       |""".stripMargin
    )
  }
  @withNamedToString
  case class Foo(bar: Int, baz: String)

  @withNamedToString
  case class FooHidden(bar: Int, @hidden baz: String)

  @withNamedToString
  case class Person(name: String, age: Int, email: String)

  @withNamedToString
  case class Empty()

  @withNamedToString
  case class Single(value: Int)

  @withNamedToString
  case class WithCompanion(x: Int, y: String)

  object WithCompanion {
    def create(x: Int): WithCompanion = WithCompanion(x, "default")
  }

}
