# Fakeo DB

An attempt to replicate Linq/MongoDB behaviors in GDScript.


## Features

* Filter lists using custom queries
* Deferred execution
* Easy to extend operators and enumerables
* Query lists of objects and primitives


## Quick Example

```gdscript
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = FakeoDB.OperatorFactory

# Combine objects and dictionaries
var data = [
	Weapon.new("Wooden Sword", "melee", "sword", 2, 1.2),
	...
	Weapon.new("Bone Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	...
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
]

# select all where subtype is 'bow' and dmg >=7. 
# Project to dictionary with {"name", "dmg", "atk_range"} as fields.
var query = fdb.list(data)\
	.where({subtype=ops.eq("bow"), dmg=ops.gteq(7)})\
	.project(["name", "dmg", "atk_range"])

func _run():
	print(query.to_array()) 
	# OR
  	for i in query: # in a for loop
		  print(i)
```

### And...

```gdscript
# this
var query = fdb.list.new(data).where(comparers).project(fields)
```

```gdscript
# is the same as
var qb = fdb.QueryBuilder.new().where(comparers).project(fields)
query = qb.eval(data)
```

```gdscript
# which is the same as
var list = fdb.list.new(data)
var where = fdb.Enumerables.Where.new(list, comparers)
var project = fdb.Enumerables.Project.new(where, fields)
```

### Also...

```gdscript
var query = fdb.list(data)\
	.first({subtype=ops.eq("bow"), dmg=ops.gteq(7)})
```

```gdscript
# is the same as
func _bow_condition(item):
	return item.subtype == "bow" and item.dmg >= 7

var query = fdb.list(data)\
	.first(funcref(self, "_bow_condition"))
```

## Overview
### [Enumerables](../master/addons/fakeo_db/scripts/Enumerables.gd)

All inherit from base Enumerable class
* List - wrap a list with this to use where, project etc.
* Collection - an enumerable with append, erase functions and signals on change
* Where - get all satisfying a condition. 
  * Accepts functions, dictionaries and Operators
  * Works with lists of primitives and objects
* Project - project chosen fields into dictionary
* Take - take first N results
* Select - apply a function to each item and return the result

* count(), at(index), first(pred), any(pred) can be called from children of Enumerable class.
  * note that these will cause immediate evaluation
  * like Where; first and any both accept FuncRef, Dictionary and Operator types as predicates

#### About Where

When using where, any or first; the supplied predicate must match the data.
funcref and object variations work with all data formats. The dictionary of operators approach only applies to lists of objects or dictionaries.

```gdscript

var numbers = fdb.list([1,2,3,4,5])
var employees = fdb.list([{name="Al", age=44, salary=20000}, Item.new("Jackie", 23, 40000)])\

# VALID

# fine, single operator expects single argument
numbers.where(ops.and([ops.gt(1), ops.lt(4)]))

# funcref taking one argument
func _is_between_1_4(item):
	return item > 1 and item < 4
numbers.where(funcref(self, "_is_between_1_4"))

# valid if class has eval(item) function or extends OperatorBase
numbers.where(IsBetween.new(1,4))

employees.where({age=ops.gt(30))


# NOT VALID

# will look for 'value' field in an int
fdb.list([1,2,3,4,5]).where({value=ops.eq(5)})

# single operator to object, will compare entire entry
# i.e. {age=44, salary=20000} > 30 ??
employees.where(ops.gt(30))
```

### [Operators](../master/addons/fakeo_db/scripts/Operators.gd)

Operators take a single value (in 'eval(item)') and return a boolean

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