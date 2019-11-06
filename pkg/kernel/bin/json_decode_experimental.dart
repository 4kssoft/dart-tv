import 'package:json_transformer/json_transformer.dart';

class User<K, V, L> {
  final String name;
  final int age;
  final List<L> extras;
  final Map<K, V> mapExtras;

  User({this.name, this.age, this.extras, this.mapExtras});

  String toString() => '''
name: $name
age: $age
extras: $extras
mapExtras: $mapExtras
''';
}

class Login<K, V, L> {
  final User<K, V, L> user;
  final String password;

  Login({this.user, this.password});

  String toString() => '''

  user:
${user.toString().split('\n').map((l) => '    $l').join('\n')}
  password: $password
''';
}

void main() {
  var users = jsonDecodeExperimental<List<User>>([
    {
      'name': 'Jack',
      'age': 10,
      'extras': [1],
      'mapExtras': {'cool': 0},
    },
  ]);
  print(users.runtimeType);

  // var decoded = jsonDecodeExperimental<
  //     Map<String, List<Login<int, dynamic, User<String, int, int>>>>>({
  //   'a': [
  //     {
  //       'user': {
  //         'name': 'Jack',
  //         'age': 10,
  //         'extras': [
  //           {
  //             'name': 'Jack',
  //             'age': 10,
  //             'extras': [1],
  //             'mapExtras': {'cool': 0},
  //           }
  //         ],
  //         'mapExtras': {
  //           1: 'cool',
  //           3: false,
  //         }
  //       },
  //       'password': 'adm1n'
  //     },
  //   ],
  // });
  // print(decoded);
  // print(decoded['a'][0].user.extras[0].runtimeType);

  // final data = {
  //   'name': 'Jack',
  //   'age': 10,
  //   'extras': ['foo', 1],
  //   'mapExtras': {'foo': 1},
  // };
  // var user = jsonDecodeExperimental<User>(data);
  // var decoded =
  //     jsonDecodeExperimental<Map<String, List<Login<int, dynamic, bool>>>>({
  //   'a': [
  //     {
  //       'user': {
  //         'name': 'Jack',
  //         'age': 10,
  //         'extras': [true, false],
  //         'mapExtras': {
  //           1: 'cool',
  //           3: false,
  //         }
  //       },
  //       'password': 'adm1n'
  //     },
  //   ],
  // });
  // print(decoded);
  // print(decoded);
  // print(decoded['a'][0].user.runtimeType);
  // final data = {
  //   'name': 'Jack',
  //   'age': 10,
  //   'extras': ['foo', 'bar']
  // };
  // var user = jsonDecodeExperimental<User<String>>(data);
  // print(user.name);
  // var login = jsonDecodeExperimental<Login>({
  //   'user': {
  //     'name': 'Jack',
  //     'age': 10,
  //     'extras': [1, 2]
  //   },
  //   'password': 'adm1n'
  // });
  // print(login.user.name);
  // User inferred = jsonDecodeExperimental({'name': 'Joe', 'age': 10});
  // print(inferred.name);
  // user = jsonDecodeExperimental(data);
  // print(user.name);
  // print(Function.apply(User.create, ['Jose']).name);
  // var gross = jsonDecodeExperimental({'user': 'John', 'age': 10});
  // print(gross);
}
