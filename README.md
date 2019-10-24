# Godot - Easy Queries

A library that offers Linq/MongoDB style data operations in Godot written in GdScript.

## Usage

Enumerables offer deferred execution and can be iterated over in a for loop.

Comparers are used to filter fields.

There is also the QueryBuilder class that allows for queries to be created without a predefined list.

At present only dictionaries and lists of objects are compatible but this will change in the future.

### Quick Example
```gdscript
const OpFac = EasyQueries.OperatorFactory

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
var query = EasyQueries.ListEnumerator.new(data)\
	.where({subtype=OpFac.eq("bow")})\
	.project(["name", "dmg", "atk_range"])

func _run():
  	for i in query:
		  print(i)
```

### And...

```gdscript
# this
var query = EasyQueries.ListEnumerator.new(data).where(comparers).project(fields)
```

```gdscript
# is the same as
var query = EasyQueries.QueryBuilder.new().where(comparers).project(fields)
query.eval(data)
```

```gdscript
# which is the same as
var list = List.new(data)
var where = EasyQueries.Enumerables.Where.new(list, comparers)
var project = EasyQueries.Enumerables.Project.new(where, fields)

```

## Overview
### Enumerables
* ListEnumerator - wrap a list with this
* Where - get all satisfying a condition
* Project - project chosen fields into dictionary
* Take - take first N results
* Select - get values in a form specified by the user

* count(), first() and at(index) can be called on enumerator
 * note that these will cause immediate evaluation

### Operators
* HasField

* And
* Or

* Not

* Any
* None

* LT
* GT
* Eq

* In
* Contains

* CmpFunction
* CmpFunctionWithArgs


## Installation

1. Either download from the Godot Asset Store or just copy 'addons/easy_queries' into the 'addons' folder in your project.

2. Project > Project Settings > Plugins (Tab at the top) > EasyQueries

## Planned features

* Documentation/Wiki
* move to a generally more OO approach
  * more OO operator/filter system
* ability to use any list as argument
* godot editor-interface
* Mongo style Databases and Collections
* update operations - add, remove, update
* foreach
* ability to create Enumerables as searchable nodes
* tests

