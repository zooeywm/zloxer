# Lox programming language

Learn from <https://www.craftinginterpreters.com>

Run `cargo doc --open --document-private-items` to view rustdoc.

`cargo test` to run tests.

`cargo run -- help` to show help.

`cargo run -- repl` to run reactive interpreter.

`cargo run -- file <path>` to run lox file.

`cargo run -- file tests/test.lox` to run the test lox file.

## Concepts

### Dynamic typing

Lox is dynamically typed. Variables can store values of any type, and a single variable can even store values of different types at different times. If you try to perform an operation on values of the wrong type—say, dividing a number by a string—then the error is detected and reported at runtime.

### Automatic memory management

garbage collection(GC)

## Data Types

### `Boolean`

- `true`
- `false`

### `Number`

only `double`

```lox
1234;  // An integer.
12.34; // A decimal number.
```

### `String`

```lox
"I am a string";
"";    // The empty string
"123"; // This is a string, not a number
```

### `Nil`

Represents "no value"

## Expressions

To produce a `value`

### Arithmetic

binary operator

- `+`
- `-`
- `*`
- `/`

negative number

`-` `Number`

concatenate string

`String` `+` `String`

### Comparison and equality

`Number` > `Number`

`Number` < `Number`

`Number` >= `Number`

`Number` <= `Number`

`Number` == `Number`

`Number` != `Number`

`String` == `String`

`String` != `String`

### Logical operators

- `!`: negative
- `and`: logical and
- `or`: logical or

if the left operand of an or is true, the right is skipped.

### Precedence and grouping

All of these operators have the same precedence and associativity that you’d expect coming from C.
In cases where the precedence isn’t what you want, you can use () to group stuff.

```lox
var average = (min + max) / 2;
```

### Statements

To produce an `effect`

An expression followed by a semicolon (;) promotes an expression to statement-hood.

If you want to pack a series of statements where a single one is expected, you can wrap them up in a block.

```lox
{
  print "One statement.";
  print "Two statements.";
}
```

Blocks also affect scoping.

### Variables

You declare variables using `var` statements. If you omit the initializer, the variable’s value defaults to `nil`.

```lox
var imAVariable = "here is my value";
var iAmNil;
```

Once declared, you can, naturally, access and assign a variable using its name.

```lox
var breakfast = "bagels";
print breakfast; // "bagels".
breakfast = "beignets";
print breakfast; // "beignets".
```

### Control Flow

An if statement executes one of two statements based on some condition.

```lox
if (condition) {
  print "yes";
} else {
  print "no";
}
```

A while loop executes the body repeatedly as long as the condition expression evaluates to true.

```lox
var a = 1;
while (a < 10) {
  print a;
  a = a + 1;
}
```

Finally, we have for loops.

```lox
for (var i = 0; i < 10; i = i + 1) {
  print i;
}
```

### Functions

A function call expression looks the same as it does in C.

```lox
makeBreakfast(bacon, eggs, toast);
```

You can also call a function without passing anything to it.

```lox
makeBreakfast();
```

Define function with `fun`

```lox
fun printSum(a, b) {
  print a + b;
}
```

- An `argument` is an actual value you pass to a function when you call it. So a function call has an argument list.

- A `parameter` is a variable that holds the value of the argument inside the body of the function. Thus, a function declaration has a parameter list. Others call these formal parameters or simply formals.

The body of a function is always a block. Inside it, you can return a value using a return statement.

```lox
fun returnSum(a, b) {
  return a + b;
}
```

If execution reaches the end of the block without hitting a `return`, it implicitly returns `nil`.

### Closures

Functions are first class in Lox, which just means they are real values that you can get a reference to, store in variables, pass around, etc. This works:

```lox
fun addPair(a, b) {
  return a + b;
}

fun identity(a) {
  return a;
}

print identity(addPair)(1, 2); // Prints "3".
```

Since function declarations are statements, you can declare local functions inside another function.

```lox
fun outerFunction() {
  fun localFunction() {
    print "I'm local!";
  }

  localFunction();
}
```

If you combine local functions, first-class functions, and block scope, you run into this interesting situation:

```lox
fun returnFunction() {
  var outside = "outside";

  fun inner() {
    print outside;
  }

  return inner;
}

var fn = returnFunction();
fn();
```

Here, inner() accesses a local variable declared outside of its body in the surrounding function.

For that to work, inner() has to “hold on” to references to any surrounding variables that it uses so that they stay around even after the outer function has returned. We call functions that do this closures. These days, the term is often used for any first-class function.

### Classes

You declare a class and its methods like so:

```lox
class Breakfast {
  cook() {
    print "Eggs a-fryin'!";
  }

  serve(who) {
    print "Enjoy your breakfast, " + who + ".";
  }
}
```

The body of a class contains its methods. They look like function declarations but without the fun keyword. When the class declaration is executed, Lox creates a class object and stores that in a variable named after the class. Just like functions, classes are first class in Lox.

```lox
// Store it in variables.
var someVariable = Breakfast;

// Pass it to functions.
someFunction(Breakfast);
```

Next, we need a way to create instances. We could add some sort of new keyword, but to keep things simple, in Lox the class itself is a factory function for instances. Call a class like a function, and it produces a new instance of itself.

```lox
var breakfast = Breakfast();
print breakfast; // "Breakfast instance".
```

### Instantiation and initialization

Lox, like other dynamically typed languages, lets you freely add properties onto objects.

```lox
breakfast.meat = "sausage";
breakfast.bread = "sourdough";
```

If you want to access a field or method on the current object from within a method, you use good old `this`.

```lox
class Breakfast {
  serve(who) {
    print "Enjoy your " + this.meat + " and " +
        this.bread + ", " + who + ".";
  }

  // ...
}
```

Part of encapsulating data within an object is ensuring the object is in a valid state when it’s created. To do that, you can define an initializer. If your class has a method named init(), it is called automatically when the object is constructed. Any parameters passed to the class are forwarded to its initializer.

```lox
class Breakfast {
  init(meat, bread) {
    this.meat = meat;
    this.bread = bread;
  }

  // ...
}

var baconAndToast = Breakfast("bacon", "toast");
baconAndToast.serve("Dear Reader");
// "Enjoy your bacon and toast, Dear Reader."
```

### Inheritance

Every object-oriented language lets you not only define methods, but reuse them across multiple classes or objects. For that, Lox supports single inheritance. When you declare a class, you can specify a class that it inherits from using a less-than (<) operator.

```lox
class MyBreakfast < Breakfast {
  drink() {
    print "How about a Bloody Mary?";
  }
}
```

Here, MyBreakfast is the derived class or subclass, and Breakfast is the base class or superclass.

Every method defined in the superclass is also available to its subclasses.

```lox
var benedict = MyBreakfast("ham", "English muffin");
benedict.serve("Noble Reader");
```

Even the init() method gets inherited. In practice, the subclass usually wants to define its own init() method too. But the original one also needs to be called so that the superclass can maintain its state. We need some way to call a method on our own instance without hitting our own methods.

```lox
class MyBreakfast < Breakfast {
    init(meat, bread, drink) {
    super.init(meat, bread);
    this.drink = drink;
    }
}
```

### The Standard Library

This is the saddest part of Lox. Its standard library goes beyond minimalism and veers close to outright nihilism. For the sample code in the book, we only need to demonstrate that code is running and doing what it’s supposed to do. For that, we already have the built-in print statement.

Later, when we start optimizing, we’ll write some benchmarks and see how long it takes to execute code. That means we need to track time, so we’ll define one built-in function, clock(), that returns the number of seconds since the program started.

And . . . that’s it. I know, right? It’s embarrassing.

If you wanted to turn Lox into an actual useful language, the very first thing you should do is flesh this out. String manipulation, trigonometric functions, file I/O, networking, heck, even reading input from the user would help. But we don’t need any of that for this book, and adding it wouldn’t teach you anything interesting, so I’ve left it out.

Don’t worry, we’ll have plenty of exciting stuff in the language itself to keep us busy.
