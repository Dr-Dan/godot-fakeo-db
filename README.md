# Fakeo DB

Linq/MongoDB stuff in Godot.

## Features

* Filter lists using custom queries
* Deferred execution of queries
* Easy to extend with custom operators and enumerables

Note: At present only lists of dictionaries and objects are compatible but this will change in the future. 

### Quick Example
```gdscript
const OpFac = FakeoDB.OperatorFactory

var data = [
	Weapon.new("Wooden Sword", "melee", "sword", 2, 1.2),
	Weapon.new("Nice Spear", "melee", "spear", 13, 2.0),
	Weapon.new("Jan's Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_range=20.0, firing_rate=1.4},
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_range=20.0, firing_rate=1.7}
]

# select all where subtype is 'bow'. Project to dictionary with {"name", "dmg", "atk_range"} as fields.
var query = FakeoDB.ListEnumerator.new(data)\
	.where({subtype=OpFac.eq("bow")})\
	.project(["name", "dmg", "atk_range"])

func _run():
  	for i in query:
		  print(i)
```

### And...

```gdscript
# this
var query = FakeoDB.ListEnumerator.new(data).where(comparers).project(fields)
```

```gdscript
# is the same as
var qb = FakeoDB.QueryBuilder.new().where(comparers).project(fields)
query = qb.eval(data)
```

```gdscript
# which is the same as
var list = List.new(data)
var where = FakeoDB.Enumerables.Where.new(list, comparers)
var project = FakeoDB.Enumerables.Project.new(where, fields)

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

2. Enable plugin: Project > Project Settings > Plugins

## Planned features

* Better examples
* ability to search any list i.e. ints, strings
* godot in-editor interface for database
* update operations - add, remove, update
* tests