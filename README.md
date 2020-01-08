# Fakeo DB

Linq/MongoDB stuff in GDScript without any database faffery.

## Features

* Filter lists using custom queries
* Deferred execution of queries
* Easy to extend with custom operators and enumerables

Note: At present only lists of dictionaries and objects are compatible i.e. no search on lists of primitives

## Quick Example

```gdscript
const Fakeo = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const OpFac = FakeoDB.OperatorFactory

var data = [
	Weapon.new("Wooden Sword", "melee", "sword", 2, 1.2),
	...
	Weapon.new("Bone Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	...
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
]

# select all where subtype is 'bow' and dmg >=7. Project to dictionary with {"name", "dmg", "atk_range"} as fields.
var query = FakeoDB.List.new(data)\
	.where({subtype=OpFac.eq("bow"), dmg=OpFac.gteq(7)})\
	.project(["name", "dmg", "atk_range"])

func _run():
	print(query.to_list()) 
	# OR
  	for i in query: # in a for loop
		  print(i)
```

### And...

```gdscript
# this
var query = List.new(data).where(comparers).project(fields)
```

```gdscript
# is the same as
var qb = QueryBuilder.new().where(comparers).project(fields)
query = qb.eval(data)
```

```gdscript
# which is the same as
var list = List.new(data)
var where = Enumerables.Where.new(list, comparers)
var project = Enumerables.Project.new(where, fields)
```

## Overview
### [Database](../master/addons/fakeo_db/scripts/Database)
* Collection - an enumerable with append, erase functions and signals
* Database - Stores collections

### [Enumerables](../master/addons/fakeo_db/scripts/Enumerables.gd)
* List - wrap a list with this to use where, project etc.
* Where - get all satisfying a condition
* Project - project chosen fields into dictionary
* Take - take first N results
* Select - get values in a form specified by the user

* count(), first() and at(index) can be called from enumerables
  * note that these will cause immediate evaluation

### [Operators](../master/addons/fakeo_db/scripts/Operators.gd)

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

1. Either download from the Godot Asset Store or just copy fakeo_db into the 'addons' folder in your project.

2. (Optional) Enable plugin: Project > Project Settings > Plugins

## Planned features

* Better examples
* ability to search any list i.e. ints, strings
* godot in-editor interface for database
* update operations - add, remove, update
* tests