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

# ==============================================================;
# Three ways

# nested
var list = fdb.list(data)
# There are 3 types of Where Enumerable. 
# You may want to check the Enumerables file if you plan on using this method.
var where = fdb.Enumerables.WhereDict.new(list, {dmg=ops.gteq(10)})
var project = fdb.Enumerables.Project.new(where, ["name", "dmg"])
var take = fdb.Enumerables.Take.new(project, 3)

func get_name_and_dmg_mult(item, mult):
	return [item.name, item.dmg * mult]

var select = fdb.Enumerables.Select.new(where, funcref(self, "get_name_and_dmg_mult"), [20])

# -------------------------------------------------
# the where function in the chaining method determines the type of Where function required
# from the argument type: Dictionary->WhereDict, FuncRef->WhereFunc, OperatorBase->WhereOp
# enumerable + chaining
var query = fdb.list(data)\
	.where({subtype="bow", dmg=ops.gteq(7)})\
	.project(["name", "dmg", "atk_range"])

# query builder + chaining (eval() later...)
var query2 = fdb.QueryBuilder.new()\
	.where({subtype=ops.in_(["sword", "spear", "thrown"])})\
	.project(["name", "subtype", "dmg", "atk_range"])
		
# using a funcref
# this can also be used in the 'first' and 'exists' functions of Enumerator class
func _damage_or_sword(item):
	return item.dmg > 20 or item.name=="Wooden Sword"
	
# can use qb() in place of QueryBuilder.new()
var query3 = fdb.qb()\
	.where(funcref(self, "_damage_or_sword"))\
	.project(["name", "subtype", "dmg",])
# ==============================================================


func _run():
	var queries = [
		take,
		select,
		query,
		# must call eval on query builder
		query2.eval(data),
		query3.eval(data),
	]
	
	for q in queries:
		print_break_mini()
		q.for_each(self, "_print")
#		print(q.to_array()) 

	print_break_mini()
	print(take.at(1).name)

		
# ==============================================================

func _print(msg):
	print(msg)
	
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")