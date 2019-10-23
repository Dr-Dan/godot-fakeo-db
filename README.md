# Godot-EzDatabase

A library that replicates Linq/MongoDB style data access in Godot written in GdScript.


## Usage

Enumerables offer deferred execution and can be iterated over in a for loop.

At present only dictionaries and lists of objects are compatible but this will change in the future.

There is also the QueryBuilder class that allows for queries to be created without a predefined list.

### Quick Example
```
const List = EnumeratorsDeferred.ListEnumerator

var data = [
	Weapon.new("Wooden Sword", "melee", "sword", 2, 1.2),
	Weapon.new("Katana", "melee", "sword", 16, 1.6),
	Weapon.new("Nice Spear", "melee", "spear", 13, 2.0),
	Weapon.new("Jan's Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_range=20.0, firing_rate=1.4},
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
]

# select all where subtype is bow. Project to dictionary with {"name", "dmg", "atk_range"} as fields.
var query = List.new(data)\
	.where({subtype=OpFac.eq("bow")})\
	.project(["name", "dmg", "atk_range"])

func _run():
  	for i in query:
		  print(i)
```

### Also

```
# this
var query = EnumeratorsDeferred.ListEnumerator.new(data).where(comparers).project(fields)
```

```
# is the same as
var query = QueryBuilder.new().where(comparers).project(fields)
query.eval(data)
```

```
# which is the same as
var list = List.new(data)
var where = EnumeratorsDeferred.Where.new(list, comparers)
var project = EnumeratorsDeferred.Project.new(where, fields)

```


### Enumerable Overview
* ListEnumerator - wrap a list with this
* Where - get all satisfying a condition
* Project - project chosen fields into dictionary
* Take - take first N results
* Select - get values in a form specified by the user


## Planned features

* Documentation/Wiki
* more OO operator/filter system
* ability to use any list as argument
* Mongo style Databases and Collections
* update operations - add, remove, update
* foreach
* ability to create Enumerables as searchable nodes
* tests

