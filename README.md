#PLCInterpreter

This interpreter was a semester-long project for the class **Programming Language Concepts (EECS 345)** at Case Western Reserve University. Almost all development was done tri-programming, which turned out to be extremely helpful for this project because it proved to be really tricky to design correctly (at least for us newbies) and tricky to make sense of the debugging data. Each individual would have eureka moments when everyone else was stuck that helped drive progress forward!

There were many different stages of functionality that the interpreter had to achieve at different times during the courses. I trimmed the files down with the final results in mind only.

## Usage

### From Racket

```racket
(require "interpreter.rkt")
(interpret "/path/to/program_src" 'MainClassName)
```

### From the command line

```shell
./interpreter.sh /path/to/program_src MainClassName
```
##Sample Programs

#### Hello Wor... err. Hello Zero (test/objects/0)
```
class A {
  static function main() {
    return 0;
  }
}
```
```
$ ./interpret.sh test/objects/0 A
0
```

#### "Absolute Tomfoolery" (test/objects/13)
```
class A {
  var count = 0;

  function subtract(a, b) {
    if (a < b) {
       throw b - a;
    }
    else
       return a - b;
  }
}

class B extends A {
  function divide(a, b) {
    if (b == 0)
      throw a;
    else
      return a / b;
  }

  function reduce(a, b) {
    while (a > 1 || a < -1) {
      try {
        a = divide(a, b);
        if (a == 2)
          break;
      }
      catch (e) {
        return subtract(a, b); 
      }
      finally {
        count = count + 1;
      }
    }
    return a;
  }
}

class C {
  function main() {
    var x;
    var b;

    b = new B();

    try {
      x = b.reduce(10, 5);
      x = x + b.reduce(81, 3);
      x = x + b.reduce(5, 0);
      x = x + b.reduce(-2, 0);
      x = x + b.reduce(12, 4);
    }
    catch (a) {
      x = x * a;
    }
    finally {
      x = -1 * x;
    }
    return x - b.count * 100;
  }
}
```
```
$ ./interpret.sh test/objects/13 C
-516
```

## Try it out

Requires the `racket` binary on your `PATH` environment variable

```shell
$ git clone https://github.com/MarkLalor/PLCInterpreter.git && cd PLCInterpreter
$ ./interpret.sh test/objects/12 List
5285
```