main() {
  var namedArgs = {#b: '2'};
  Function.apply(foo, [1], namedArgs);
  Function.apply(bar, [1], namedArgs);
}

dynamic get bar => foo;

void foo(int a, {String b}) {
  print('a: $a\nb: $b');
}
