tool
extends EditorScript

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory


"""
To use: File > Run
"""

class Weapon:
	var name
	var type
	var subtype
	var dmg
	var atk_range

	func _init(name ,type ,subtype ,dmg ,atk_range):
		self.name = name
		self.type = type
		self.subtype = subtype
		self.dmg = dmg
		self.atk_range = atk_range
		
class WeaponRanged:
	extends Weapon
	
	var firing_rate = 0

	func _init(name ,type ,subtype ,dmg ,atk_range, _firing_rate).(name ,type ,subtype ,dmg ,atk_range):
		firing_rate = _firing_rate
	
	
var data = fdb.list([
	{name="Wooden Sword", type="melee", subtype="sword", dmg=2, atk_range=1.2},
	Weapon.new("Katana", "melee", "sword", 16, 1.6),
	Weapon.new("Nice Spear", "melee", "spear", 13, 2.0),
	Weapon.new("Jan's Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_range=20.0, firing_rate=1.4},
	WeaponRanged.new("Glass Bow", "ranged", "bow", 7, 20.0, 1.7),
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
])

# ==============================================================;

const E = fdb.Enumerables
# You may want to check out the Enumerables.gd if you plan on using this method.
var where = E.WhereOp.new(data, ops.dict({dmg=ops.gteq(10)}))
var project = E.Project.new(where, ["name", "dmg"])
var take = E.Take.new(project, 3)

func get_name_and_dmg_mult(item, mult):
	return [item.name, item.dmg * item.atk_range, mult]
	
var select = E.Select.new(where, funcref(self, "get_name_and_dmg_mult"), [20])

# -------------------------------------------------
# the where function in the chaining method determines the type of Where function required
# from the argument type: Dictionary->WhereDict, FuncRef->WhereFunc, OperatorBase->WhereOp
var chain_queries = [
	{
		description="project fields from each object",
		query=data\
			.project(["name", "type", "subtype", "dmg"])
	},
	{
		description="where: dmg >= 10",
		query=where
	},
	{
		description="project: name and dmg fields",
		query=project
	},
	{
		description="take: 3 from projected result",
		query=take
	},
	{
		description="select: multiply damage",
		query=select
	},
	{
		description="bows with dmg >= 7",
		query=data\
			.where({subtype="bow", dmg=ops.gteq(7)})\
			.project(["name", "dmg", "atk_range"])
	}
]
# using a funcref
# this can also be used in the 'first' and 'exists' functions of Enumerator class
func _damage_or_sword(item):
	return item.dmg > 20 or item.name=="Wooden Sword"

var build_queries= [
	{
		description="subtype is in [sword, spear, thrown]",
		query=fdb.QueryBuilder.new()\
			.where({subtype=ops.in_(["sword", "spear", "thrown"])})\
			.project(["name", "subtype", "dmg", "atk_range"])
	},
	{
		description="filter by dmg and type",
		# can use qb() in place of QueryBuilder.new()
		query=fdb.qb()\
			.where(funcref(self, "_damage_or_sword"))\
			.project(["name", "subtype", "dmg",])
	},
	{
		description="item damage >= 10 and 'o' in subtype",
		# using a string in where() will execute it as code (using the Expression class)
		query=fdb.qb()\
			.where('_item.dmg >= 10 and "o" in _item.subtype')\
			.project(["name", "subtype", "dmg",])
	}
]
	
# ==============================================================

func _run():
	for q in chain_queries:
		print_break_mini()
		print(q.description)
#		q.query.for_each(funcref(self, "_print"))
		for item in q.query:
			print(item)
		
	for q in build_queries:
		print_break_mini()
		print(q.description)
		var result = q.query.eval(data)
		result.for_each(funcref(self, "_print"))

# ==============================================================

func _print(msg):
	print(msg)
	
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")
