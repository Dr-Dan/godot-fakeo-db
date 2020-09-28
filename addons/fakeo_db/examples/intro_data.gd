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
var flex = fdb.flex()

func _run():
		
	ex_util.pr_equals_break()
	flow()
	
	ex_util.pr_equals_break()
	print("using a collection of objects (Dictionary)\n")
	open_operators()

	ex_util.pr_equals_break()
	sorting()

	ex_util.pr_equals_break()
	more_processors()
	
	ex_util.pr_equals_break()	
	example_enumerate()

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
	{name="Jane Doe", age=16, addr_id=1, inv={weapon={knife=2}, food={}}},
	{name="xXx", age=42, addr_id=1, inv={money={coin=2000}, weapon={knife=10}, food={}, drink={relentless=1}}},
	Human.new("Carla", 49, 2, {money={coin=248}, food={berry=20}}),
]

var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250}]

func flow():
	ex_util.pr_array('first 3', 
		fdb.apply(prc.take(3), name_table))

	ex_util.pr_array('skip 3, get name', fdb.apply(
		[prc.skip(3), prc.map(ops.open('name'))], 
		name_table))
	
	ex_util.pr_array('take_while ...', fdb.apply(
		prc.take_while(ops.expr('_x.age > 20')), 
		name_table))

	# Iterable does not execute immediately
	var it = fdb.itbl(
		[prc.skip(1), prc.take(4)], 
		name_table)

	# instead it does so when calling run, at, first etc.
	ex_util.pr_equals_break()
	print('iterable [skip 1, take 4]:')
	ex_util.pr_array('run:', it.run())
	# apply() the effects to a different collection
	ex_util.pr_array('apply ^^^ to some collection:', it.apply(name_table))
	printt('\n3rd: %s\n5th: %s' % [it.at(3), it.at(5)])
	printt('\nfirst: %s\nsecond: %s' % [it.front(), it.ffront()])
	printt('\nlast: %s\nsecond-last: %s' % [it.back(), it.bback()])
	printt('\nsize: %s\nempty?: %s' % [it.size(), it.empty()])

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
		if x_total == null: return x_next
		return x_next + x_total


func open_operators():
	var procs = [
		{
			# if open is called with a single arg (String) 
			#   get the value of a field from each item
			msg='all names',
			proc=prc.map(ops.open('name'))
		},
		{
			#  if called with Array
			#  	 open multiple fields; use slashes to go deeper
			#  result is a dictionary for each
			msg='view weapons, name and age',
			proc=prc.map(ops.open(['inv/weapon', 'name', 'age']))
		},
		{
			# prc.project(x) == prc.map(ops.open(x))
			# accepts Array and String as input
			msg='project name, inv',
			proc=prc.project(['name', 'inv'])
		},
		{
			msg='who has coin >= 10?',
			proc=[
				prc.filter(
					# can use slashes in dict_cmpr
					ops.dict_cmpr({'inv/money/coin':ops.gteq(10)})),				
				prc.project(['name', 'inv/money'])]
		},
		{
			# expressions follow the same principle
			msg='age:coin ratio',
			proc=[
				prc.filter(
					ops.expr('coin != null', 'inv/money/coin')),
				prc.map(ops.dict_apply({
					age_coin_ratio=ops.expr('age/float(coin)', ['inv/money/coin', 'age'])}, 
					['name']))]
		},
		{
			# use a dictionary allows mapping to fields that do not yet exist
			# 	fields that exist will be opened as in ops.open
			# if a key is not in the item: 
			#   it will be created and the relevant op will take the entire item
			# 	as an arg instead of item[key]
			msg='who has food?',
			proc=prc.map(ops.dict_apply(
				{has_food='not _x.inv.food.empty()', age=ops.expr('_x*2')}, 
				['name', 'inv/food'], 
				true))
		},
	]
	# apply expects a Processor as the first arg
	for itm in procs:
		ex_util.pr_array(itm.msg,
			fdb.apply(itm.proc, name_table))

	ex_util.pr_dash_break()
	# get all knife field data
	var knife_data = fdb.apply(
		prc.project('inv/weapon/knife'), name_table)
	# sum
	printt('total knives:',
		fdb.reduce(
			Add.new(),
			knife_data))
		
func sorting():
	print('Sorting')
	ex_util.pr_array('sort by age', 
		flex.map(
			ops.open(['name', 'age']), 
			fdb.sort(
				# in this context, additional args i.e.'age' will refer to _x
				ops.expr('age < _y.age', ['age']),
				name_table)))
			
	var coin_holders = flex.filter(ops.open('inv/money/coin'), name_table)
	ex_util.pr_array('sort by wealth', 
		flex.map(
			ops.open(['name', 'inv/money/coin']), 
			fdb.sort(
				ops.expr('_x.inv.money.coin > _y.inv.money.coin'),
			coin_holders)))
			
func more_processors():
	var value=30000
	var value_qry = prc.comp([
		prc.filter(ops.dict_cmpr({value=ops.gteq(value)})),
		prc.project(["addr_id", "street", "value"])])

	ex_util.pr_array("house value > %d" % value, 
		fdb.apply(value_qry, addr_table))

	# select just the addr_id
	var addrs = flex.map(
		ops.open('addr_id'),
		fdb.apply(value_qry, addr_table))

	# select any person where the addr_id is in addrs
	var homeowners = fdb.apply(
		prc.comp([
			prc.filter(ops.dict_cmpr({addr_id=ops.in_(addrs)})),
			prc.project(["name", "addr_id"])]),
		name_table)

	ex_util.pr_array(
		"homeowners filter house value > %d (addr_id in 'addr_ids')" % value,
		homeowners)


	var is_cls = prc.comp([
		prc.filter(ops.is_(Human)),
		prc.project(['name'])])

	# use Variant.Type enum if the class-type can't be used
	var is_dict = prc.comp([
		prc.filter(ops.is_var(TYPE_DICTIONARY)),
		prc.project(['name'])])

	ex_util.pr_array('extends Human:', fdb.apply(is_cls, name_table))
	ex_util.pr_array('extends Dictionary:', fdb.apply(is_dict, name_table))


func example_enumerate():
	# enumerate
	var en_wrap = ops.expr('{"data":_x, "id":e.next()}', {e=Enumeratee.new()})
	ex_util.pr_array('enumerated (wrap)', flex.map(en_wrap, name_table))

	var en_map = ops.dict_apply({idx=ops.expr('e.next()', {e=Enumeratee.new()})},
		['name', 'age'])
	ex_util.pr_array('enumerated (map)', flex.map(en_map, name_table))

	ex_util.pr_array('enumerated (op)', flex.map(EnumerateOp.new(), name_table))
	

class Enumeratee:
	var i = -1
	func next():
		i+=1
		return i
		
class EnumerateOp:
	extends ops.Operators.OperatorBase
	var i:=-1

	func eval(x):
		i += 1
		return [i, x]
