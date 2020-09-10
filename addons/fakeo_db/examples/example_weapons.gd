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
	
	
var data = [
	{name="Wooden Sword", type="melee", subtype="sword", dmg=2, atk_range=1.2},
	Weapon.new("Katana", "melee", "sword", 16, 1.6),
	Weapon.new("Nice Spear", "melee", "spear", 13, 2.0),
	Weapon.new("Jan's Hammer", "melee", "blunt", 30, 1.3),
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_range=20.0, firing_rate=1.4},
	{name="Crossbow", type="ranged", subtype="bow", dmg=9, atk_range=10.0, firing_rate=0.8},
	WeaponRanged.new("Glass Bow", "ranged", "bow", 7, 20.0, 1.7),
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_range=30.0, firing_rate=1.0},
]

# ==============================================================

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")

var filter = Proc.FilterOp.new(ops.dict({dmg=ops.gteq(10)}))
var map = Proc.MapOp.new(ops.open(["name", "dmg"]))
var take = Proc.Take.new(3)
		
# -------------------------------------------------
# TODO: only use deferred in this example
var chain_queries = [
	{
		description="map fields from each object into an array",
		result=fdb.mapply(data, ["name", "type", "subtype", "dmg"])			
	},
	{
		description="apply effect (*5) to each and select others into an iterator",
		result=fdb.mapi(data, {dmg='_x.dmg*5'}, ["name", "type", "subtype"])
	},
	{
		description="create a field and map others",
		result=fdb.qapply(data, fdb.qry()\
			.filter({subtype="bow"})\
			.map({dmg_range_ratio='_x.dmg/_x.atk_range'}, 
				["name", "subtype", "dmg", "atk_range"]))
	},
	{
		description="bows with dmg >= 7",
		result=fdb.iter(data, fdb.qry()\
			.filter({subtype="bow", dmg=ops.gteq(7)})\
			.proc(Proc.MapOp.new(ops.open(["name", "dmg", "atk_range"]))))
	},	
	{
		description="map: name and dmg fields",
		result=fdb.iter(data, [filter, map])
	},
	{
		description="take: 3 from projected result",
		result=fdb.iter(data, [filter, map, take])
	},

]

func _damage_or_sword(item):
	return item.dmg > 20 or item.name=="Wooden Sword"
	
var deferred_queries= [
	{
		description="subtype is in [sword, spear, thrown] using an op",
		query=fdb.qry()\
			.filter({subtype=ops.in_(["sword", "spear", "thrown"])})\
			.map(["name", "subtype", "dmg", "atk_range"])
	},
	{
		description="filter by dmg and type using func",
		query=fdb.qry()\
			.filter(funcref(self, "_damage_or_sword"))\
			.map(["name", "subtype", "dmg",])
	},	
	{
		description="item damage >= 10 and 'o' in subtype",
		query=fdb.filtq('dmg >= 10 and "o" in subtype', ["dmg", "subtype"])\
			.map(["name", "subtype", "dmg",])
	},
	{
		description="type is Weapon",
		query=fdb.filtq(ops.is_(Weapon)).map(['name'])
	},	
	{
		description="type is Dictionary; can't use Dictionary so must use Variant type",
		query=fdb.filtq(ops.is_var(TYPE_DICTIONARY)).map(['name'])
	}	
]
	

# ==============================================================

func _add(x, y):
	return {dmg=x.dmg + y.dmg}
	
func _run():
	
	print(2 in fdb.mapply(data, ops.open('dmg')))
	print(fdb.in_(2, fdb.mapi(data, ops.open('dmg'))))
	print(fdb.mapi(data, ops.open('dmg')).contains(2))
	
	# pass the item through operators in sequence
	printt('dmgs + 1', fdb.mapply(data, ops.op_iter([
		ops.open('dmg'), ops.expr('_x + 1')])))
	# reduce only works with expressions and funcrefs
	printt('reduce from iter:', fdb.reduce(
		fdb.mapi(data, ops.open('dmg')), '_x+_y'))
	printt('reduce total damage:', fdb.reduce(
		data, funcref(self, '_add'))) # can also use ops.func_(self, '_add')
	
	for q in chain_queries:
		print_break_mini()
		print(q.description)
		for item in q.result: print(item)

	for q in deferred_queries:
		print_break_mini()
		print(q.description)
		for item in fdb.iter(data, q.query): print(item)

# ==============================================================

func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")
