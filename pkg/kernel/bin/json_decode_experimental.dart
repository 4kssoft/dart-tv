import 'package:json_transformer/json_transformer.dart';

class User {
  final String name;
  final int age;

  User({this.name, this.age});

  static User create(String name) => User(name: name);
}

void main() {
  var decoded = jsonDecodeExperimental<Map<String, List<String>>>({
    'a': ['foo']
  });
  // final data = {'name': 'Jack', 'age': 10};
  // var user = jsonDecodeExperimental<User>({'name': 'John', 'age': 10});
  // print(user.name);
  // User inferred = jsonDecodeExperimental({'name': 'Joe', 'age': 10});
  // print(inferred.name);
  // user = jsonDecodeExperimental(data);
  // print(user.name);
  // print(Function.apply(User.create, ['Jose']).name);
  // var gross = jsonDecodeExperimental({'user': 'John', 'age': 10});
  // print(gross);
}
