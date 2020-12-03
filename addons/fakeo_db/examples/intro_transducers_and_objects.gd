tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const Fko = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const Ops = Fko.OpFactory
const Tdx = Fko.TdxFactory

func _run():
		
	ex_util.pr_equals_break()
	evaluation()
	
	ex_util.pr_equals_break()
	print("using a collection of objects (Dictionary)\n")
	open_operators()

	ex_util.pr_equals_break()
	sorting()

	ex_util.pr_equals_break()
	more_transducers()
	
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
		
	func speak():
		print('hi, my name is %s' % name)
		return self
	
var name_table = [
	{name="Mike", age=22, addr_id=0, inv={money={coin=25}, weapon={gun=1, knife=2}, food={nut=2}}},
	{name="The Oldest One", age=112, addr_id=0, inv={money={coin=1}, weapon={gun=1}, food={nut=2}}},
	{name="Jane Doe", age=16, addr_id=1, inv={weapon={knife=2}, food={}}},
	{name="xXx", age=42, addr_id=1, inv={money={coin=2000}, weapon={knife=10, gun=2}, food={}, drink={relentless=1}}},
	Human.new("Carla", 49, 2, {money={coin=248}, food={berry=20}}),
	Human.new("Jing", 87, 2, {money={coin=300}, weapon={knife=1}, food={berry=20}}),
]

var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250}]

# ==============================================================	

func evaluation():
	ex_util.pr_array('first 3', 
		Fko.apply(Tdx.take(3), name_table))

	ex_util.pr_array('skip 3, get name', Fko.apply(
		[Tdx.skip(3), Tdx.map(Ops.open('name'))], 
		name_table))
	
	ex_util.pr_array('take_while ...', Fko.apply(
		Tdx.take_while(Ops.expr('_x.age > 20')), 
		name_table))

	# Iterable does not execute immediately
	var it = Fko.itbl(
		[Tdx.skip(1), Tdx.take(4)], 
		name_table)

	# instead it does so when calling run, at, first etc.
	ex_util.pr_equals_break()
	print('iterable [skip 1, take 4]:')
	# run just evaluates the iterable => Array
	ex_util.pr_array('run:', it.run())
	# apply() the effects to a different collection
	ex_util.pr_array('apply ^^^ to some collection:', it.apply(name_table))

	printt('\n3rd: %s\n5th: %s' % [it.at(3), it.at(5)])
	printt('\nfirst: %s\nsecond: %s' % [it.front(), it.ffront()])
	printt('\nlast: %s\nsecond-last: %s' % [it.back(), it.bback()])
	printt('\nsize: %s\nempty?: %s' % [it.size(), it.empty()])

# ==============================================================	

func sorting():
	print('Sorting')
	var names_and_ages = Fko.apply(
		Tdx.map(
			Ops.open(['name', 'age'])), 
		name_table)
	ex_util.pr_array('sort by age', 
					 Fko.sort(
						 Ops.expr('age < _y.age', ['age']), 
						 names_and_ages))

			
	# filter will remove null entries. chain into the sort
	# chain will handle different input types automatically.
	#   This is explained in depth in the chaining example
	ex_util.pr_array('sort by wealth', 
					 Fko.chain()\
					 .filter(Ops.open('inv/money/coin'))\
					 .map(['name', 'inv/money/coin'])\
					 .sort('_x.coin > _y.coin', name_table))
	
# ==============================================================	

# an operator for reducing should implement eval2(input_next, input_accumulated)
class Add:
	extends Ops.Operators.OperatorBase
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
	var queries = [
		{
			# if open is called with a single arg (String) 
			#   get the value of a field from each item
			msg='all names',
			tdx=Tdx.map(Ops.open('name'))
		},
		{
			#  called with Array:
			#  	 open multiple fields; use slashes to go deeeper
			#  result is a dictionary for each with the (final) field name as the key
			msg='view weapons, name and age',
			tdx=Tdx.map(Ops.open(['inv/weapon', 'name', 'age']))
		},
			{
			#  if called with Dictionary
			#  	 will write results to the key
			msg='open (my) stuff',
			tdx=Tdx.map(Ops.open({my_wpns='inv/weapon', my_age='age'}))
		},
		{
			#  open_v(alue) returns just the resulting values in an Array
			# i.e. [v0, v1] instead of [{k0:v0}, {k1:v1}]
			msg='view weapons, name and age => []',
			tdx=Tdx.map(Ops.open_v(['name', 'age', 'inv/weapon']))
		},		
		{
			# Tdx.project(x) == Tdx.map(Ops.open(x))
			# accepts Array and String as input
			msg='project name, inv',
			tdx=Tdx.project(['name', 'inv'])
		},
		{
			msg='who has coin >= 250?',
			tdx=[
				Tdx.filter(
					# dict_c(o)mp(a)r(e) looks for a given field in the dictionary
					#   and validates it using an op.
					# values can be used and will be wrapped: {field=2} => {field=ops.eq(2)}
					# you can use slashes in dict_cmpr
					Ops.dict_cmpr({'inv/money/coin':Ops.gteq(250)})),				
				Tdx.project(['name', 'inv/money'])]
		},
		{
			# expressions follow the same principle
			msg='age:coin ratio',
			tdx=[
				Tdx.filter(
					Ops.expr('coin != null', 'inv/money/coin')),
				Tdx.map(Ops.dict_apply({
					age_coin_ratio=Ops.expr('age/float(coin)', ['inv/money/coin', 'age'])}, 
					['name']))]
		},
		{
			# dict_apply allows mapping to fields that do not yet exist
			# 	fields that exist will be opened as in Ops.open
			# if a key is not in the item: 
			#   it will be created and the relevant op will take the entire item
			# 	as an arg instead of item[key]
			msg='who has food?',
			tdx=Tdx.map(Ops.dict_apply(
				{has_food='not _x.inv.food.empty()'}, 
				['name', 'inv/food'], 
				true))
		},
	]
	# apply expects a Transducer as the first arg
	for itm in queries:
		ex_util.pr_array(itm.msg,
			Fko.apply(itm.tdx, name_table))

	ex_util.pr_dash_break()
	
	# get all knife field data
	var knife_data = Fko.apply(
		Tdx.project('inv/weapon/knife'), name_table)
	# sum
	printt('total knives:',
		Fko.reduce(
			Add.new(),
			knife_data))
		
# ==============================================================	

func more_transducers():
	var chain = Fko.chain()
	var value=30000
	var value_qry = Tdx.comp([
		Tdx.filter(Ops.dict_cmpr({value=Ops.gteq(value)})),
		Tdx.project(["addr_id", "street", "value"])])

	ex_util.pr_array("house value > %d" % value, 
		Fko.apply(value_qry, addr_table))

	# select just the addr_id
	var addrs = chain.map(
		Ops.open('addr_id'),
		Fko.apply(value_qry, addr_table))

	# select any person where the addr_id is in addrs
	var homeowners = Fko.apply(
		Tdx.comp([
			Tdx.filter(Ops.dict_cmpr({addr_id=Ops.in_(addrs)})),
			Tdx.project(["name", "addr_id"])]),
		name_table)

	ex_util.pr_array(
		"homeowners filter house value > %d (addr_id in 'addr_ids')" % value,
		homeowners)

	# if the last arg in call_fn is true the item is returned
	#  instead of the function return value
	var is_cls = Tdx.comp([
		Tdx.filter(Ops.is_(Human)),
		Tdx.map(Ops.call_fn('speak', [], true)),
		Tdx.project(['name'])])

	# use Variant.Type enum if the class-type can't be used
	var is_dict = Tdx.comp([
		Tdx.filter(Ops.is_var(TYPE_DICTIONARY)),
		Tdx.project(['name'])])

	ex_util.pr_array('extends Human:', Fko.itbl(is_cls, name_table))
	ex_util.pr_array('extends Dictionary:', Fko.apply(is_dict, name_table))

# ==============================================================	

func get_funcy():
	pass

# ==============================================================

class Enumeratee:
	var i = -1
	func next():
		i+=1
		return i
	
class EnumerateOp:
	extends Ops.Operators.OperatorBase
	var i:=-1

	func eval(x):
		i += 1
		return [i, x]

func example_enumerate():
	var chain = Fko.chain()
	# enumerate
	var en_wrap = Ops.expr('{"data":_x, "id":e.next()}', {e=Enumeratee.new()})
	ex_util.pr_array('enumerated (wrap)', chain.map(en_wrap, name_table))

	var en_map = Ops.dict_apply({idx=Ops.expr('e.next()', {e=Enumeratee.new()})},
		['name', 'age'])
	ex_util.pr_array('enumerated (map)', chain.map(en_map, name_table))

	ex_util.pr_array('enumerated (op)', chain.map(EnumerateOp.new(), name_table))
	

