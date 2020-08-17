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
# You may want to check out the Processors.gd if you plan on using this method.
var filter = Proc.FilterOp.new(ops.dict({dmg=ops.gteq(10)}))
var map = Proc.MapOp.new(ops.open(["name", "dmg"]))
var take = Proc.Take.new(3)

func dmg_mult_range(item, mult):
	return {name=item.name, dmg_by_range=item.dmg * item.atk_range * mult}
	
# this doesnt make sense
var select_qry = fdb.qry([filter])\
	.proc(Proc.MapOpAuto.new(funcref(self, "dmg_mult_range"), [1.5]))

# -------------------------------------------------
var chain_queries = [
	{
		description="map fields from each object into an array",
		result=fdb.qry()\
			.map(["name", "type", "subtype", "dmg"])\
			.apply(data)
	},
	{
		description="apply effect to fields and map remaining",
		result=fdb.mapi(data, {dmg='_item.dmg*5'}, ["name", "type", "subtype"])
	},	
	{
		description="create a field and map others",
		result=fdb.filtqi(data, 
			{subtype="bow"})\
			.map(
				{dmg_range_ratio='_item.dmg/_item.atk_range'}, 
				["name", "subtype", "dmg", "atk_range"])
	},		
	{
		description="map: name and dmg fields",
		result=fdb.iter(data, [filter, map])
	},
	{
		description="take: 3 from projected result",
		result=fdb.iter(data, [filter, map, take])
	},
	{
		description="map: multiply damage",
		result=fdb.iter(data, select_qry)
	},
	{
		description="bows with dmg >= 7",
		result=fdb.qry_iter(data)\
			.filter({subtype="bow", dmg=ops.gteq(7)})\
			.proc(Proc.MapOp.new(ops.open(["name", "dmg", "atk_range"])))
	}
]

func _damage_or_sword(item):
	return item.dmg > 20 or item.name=="Wooden Sword"
	
var deferred_queries= [
	{
		description="subtype is in [sword, spear, thrown]",
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
	}
]
	

# ==============================================================

func _run():
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
