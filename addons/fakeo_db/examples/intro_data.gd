tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory
const prc = fdb.ProcessorFactory

func _run():
	ex_util.print_break()
	print("using a collection of objects (Dictionary)\n")
	open_operators()
	
	ex_util.print_break()
	filters()
# ==============================================================	
# DATA
# ==============================================================	
class Human:
	var name: String
	var age: int
	var addr_id: int
	var inv:Dictionary
	var idx = -1
	
	func _init(name, age, addr_id, inv):
		self.name = name
		self.age = age
		self.addr_id = addr_id
		self.inv = inv
	
var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="The Oldest One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}, food={nut=2}}},
	{name="Jane Doe", age=16, addr_id=1, inv={money={coin=2}, weapon={knife=2}, food={}}},
	{name="xXx", age=42, addr_id=1, inv={money={coin=2000}, weapon={knife=10}, food={}, drink={relentless=1}}},
	Human.new("Carla", 49, 2, {food={berry=20}}),
]

var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250}]

# ==============================================================
# TODO: a breezy filter example + logic i.e. not, or, and, is etc
# ==============================================================

# an operator for reducing should implement eval2(input_next, input_accumulated)
class Add:
	extends ops.Operators.OperatorBase
	var val
	
	func _init(val_=0):
		val = val_

	func eval(x):
		return x + val
			
	func eval2(x_next, x_total):
		# if no item found, do nothing
		if x_next == null: return x_total
		return x_next + x_total


func open_operators():
	# get the value of a field from each item
	#   if open is called with a single arg (String) 
	printt('names',
		fdb.mapply(ops.open('name'), name_table))
		
	# open multiple fields; use slashes to go deeper
	#   result is a dictionary for each
	#  if called with Array
	printt('view weapons, name and age',
		fdb.mapply(
			ops.open(['inv/weapon', 'name', 'age']),
			name_table))
		
	# use a dictionary allows mapping to fields that do not yet exist
	# 	fields in the array will be opened as in open
	# if a key is not in the item: 
	#   it will be created and the relevant op will take the entire item
	# 	as an arg instead of item[key]
	printt('who has food?',
		fdb.mapply(
			ops.dict_apply(
				{has_food='not _x.inv.food.empty()', age=ops.expr('_x*2')}, ['name', 'inv/food'], true),
			name_table))

	# ops.comp will exit if ops.neq returns false after any step
	var comp_op = ops.comp(
		[ops.open('inv/money/coin'), ops.gteq(10)],
		ops.neq(null))

	printt('who has coin >= 10?',
		fdb.apply(
			prc.comp([
				prc.filter(comp_op),
				prc.map(ops.open(['name', 'inv/money']))]),
			name_table))
	
	# you can write a custom reducer
	printt('total knives:',
		fdb.reduce(
			Add.new(),
			fdb.itbl(prc.map(ops.open('inv/weapon/knife')),
				name_table)))

func filters():
	var value=30000
	var value_qry = prc.comp([
		prc.filter(ops.dict_cmpr({value=ops.gteq(value)})),
		prc.map(ops.open(["addr_id", "street", "value"]))])

	# select just the addr_id
	var addrs = fdb.mapply(
		ops.open('addr_id'),
		fdb.itbl(value_qry, addr_table))

	# select any person where the addr_id is in addrs
	var homeowners = fdb.apply(
		prc.comp([
			prc.filter(ops.dict_cmpr({addr_id=ops.in_(addrs)})),
			prc.map(ops.open(["name", "addr_id"]))]),
		name_table)
#
	print("house value > %d" % value)
	print(fdb.apply(value_qry, addr_table))
#
	print("homeowners filter house value > %d (addr_id in 'addr_ids')" % value)
	print(homeowners)

	var is_cls = prc.comp([
		prc.filter(ops.is_(Human)),
		prc.map(ops.open(['name']))])

	# use Variant.Type enum if the class-type can't be used
	var is_dict = prc.comp([
		prc.filter(ops.is_var(TYPE_DICTIONARY)),
		prc.map(ops.open(['name']))])

	printt('extends Human:', fdb.apply(is_cls, name_table))
	printt('extends Dictionary:', fdb.apply(is_dict, name_table))

	# enumerate injects an incremented field ('idx') into an item
	var x = fdb.itbl(prc.enumerate('idx', 1), name_table)
	print(fdb.mapply(ops.open('idx'), x))
	
