tool
extends EditorScript

const EQ = preload("../scripts/EasyQueries.gd")
const OpFac = EQ.OperatorFactory
const List = EQ.Enumerables.ListEnumerator


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

# ==============================================================

var list = List.new(data)
var where = EQ.Enumerables.Where.new(list, {dmg=OpFac.gteq(10)})
var project = EQ.Enumerables.Project.new(where, ["name", "dmg"])
var take = EQ.Enumerables.Take.new(project, 3)

func get_name_and_dmg_mult(item, mult):
	return [item.name, item.dmg * mult]

var select = EQ.Enumerables.Select.new(where, funcref(self, "get_name_and_dmg_mult"), 20)

var query = List.new(data)\
	.where({subtype=OpFac.eq("bow")})\
	.project(["name", "dmg", "atk_range"])

var query2 = EQ.QueryBuilder.new()\
	.where({subtype=OpFac.in_(["sword", "spear", "thrown"])})\
	.project(["name", "subtype", "dmg", "atk_range"])
		
# ==============================================================
		
func _run():
	print_break()
	print_break_mini()
	for i in take:
		print(i)
		
	print_break_mini()
	print(take.at(1).name)

	print_break_mini()
	for i in query:
		print(i)
	
	print_break_mini()
	for i in query2.eval(data):
		print(i)

	print_break_mini()
	for i in select:
		print(i)		
		
# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")