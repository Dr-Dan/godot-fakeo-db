tool
extends EditorScript

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory


"""
To use: File > Run
"""

# ==============================================================

class Human:
	var name: String
	var age: int
	var addr_id: int
	
	func _init(name, age, addr_id):
		self.name = name
		self.age = age
		self.addr_id = addr_id
	
class Address:
	var addr_id: int
	var street: String
	var value: int
	
	func _init(addr_id, street, value):
		self.addr_id = addr_id
		self.street = street
		self.value = value
		
# ==============================================================
			
func _run():
	# collections are like Enumerators.List with some append and erase functions
	var name_table = [
	{name="mike", age=22, addr_id=0},
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=49, addr_id=2},
	{name="aslan", age=199, addr_id=1},
	Human.new("dan", 30, 2),
	Human.new("andy", 76, 2),
	Human.new("ariel", 65, 3),
	]

	var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250},
	Address.new(3, "diamond mews", 100000)
	]


	print_break()
	print_lists(name_table, addr_table)
	
	print_break()
	age_comp_builder_test(name_table)
#
	print_break()
	house_search_test(name_table, addr_table)
##
	print_break()
	count_names_test(name_table)
##
	print_break()
	take_test(name_table)
#
	print_break()
	collection_append_remove()
	
	print_break()
	basic()
# ==============================================================

func print_lists(name_table, addr_table):
	var query_names = fdb.iter(name_table, fdb.qry()\
		.map(["name", "age", "addr_id"]))

	var query_addr = fdb.iter(addr_table, fdb.qry()\
	.map(["addr_id", "street", "value"]))

	print("PEOPLE:")
	for i in query_names: print(i)
	print("\n--------------")
	print("ADDRESSES:")
	for i in query_addr: print(i)

# ==============================================================

var age_comp = ops.and_([ops.gt(20), ops.lt(70)]) # 20 < age < 70
var age_comp_not = ops.not_(age_comp)
var fields = ["name", "age"]

var where_age = fdb.qry()\
	.filter({age=age_comp})\
	.map(ops.open(fields))

var where_not_age = fdb.qry()\
	.filter({age=age_comp_not})\
	.map(ops.open(fields))

func age_comp_builder_test(name_table):
	print("age > 20 and age < 70")
	print(fdb.apply(name_table, where_age))
	print_break_mini()
	print("not (age > 20 and age < 70)")
	print(fdb.apply(name_table, where_not_age))

## ==============================================================

func house_search_test(name_table, addr_table, value=30000):
	var valued_houses = fdb.qry_iter(addr_table)\
		.filter({value=ops.gteq(value)})\
		.map(["addr_id", "street", "value"])

	var addr_qry = valued_houses\
		.map(ops.open('addr_id'))

	var homeowners = fdb.qry_iter(name_table)\
		.filter({addr_id=ops.in_(addr_qry)})\
		.map(["name", "addr_id"])

	print("house value > %d" % value)
	print(valued_houses.run())
	print_break_mini()
	print("homeowners filter house value > %d (addr_id in 'addr_ids')" % value)
	print(homeowners.run())

## ==============================================================

# INSTANT EVAL
func count_names_test(name_table):
	var count_name = fdb.iter(name_table, fdb.qry()\
		.filter({name=ops.contains('i')}))\
		.size()

	var result_age = fdb.qry_iter(name_table)\
		.filter({name="dan"})\
		.at(0)
		
	print("%d/%d name entries contain 'i'" % [count_name, name_table.size()])
	print("dan's age is %d" % result_age.age)

## ==============================================================

func name_starts_with_letters(name:String, letter0:String, letter1:String):
	return not name.empty() and (name[0].to_lower() == letter0 or name[0].to_lower() == letter1)
	
func to_caps(itm):
	return itm.name.to_upper()

func take_test(name_table, amt_take=4):
	var runner = fdb.qry_iter(name_table)\
		.filter({name=ops.func_(self, "name_starts_with_letters", ["a", "m"])})\
		.map({name=funcref(self, 'to_caps')})\
		.take(amt_take)

	print("take %d entries filter name starts with 'a' or 'm' (all caps): " % amt_take)
	print(runner.run())
	
# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")


# ==============================================================
# TODO: move to func example script

func _on_item_added(item):
	print("added " + str(item))

func _on_item_erased(item):
	print("removed " + str(item))

func collection_append_remove():
	var people = fdb.cltn()
	var removed = fdb.cltn()
	var added = fdb.cltn()

	var connections = [
		# functions above will be triggered
		["on_item_added", self, "_on_item_added"],
		["on_item_erased", self, "_on_item_erased"],
		# item will be added to lists (added, removed) when signal called
		["on_item_added", added, "append"],
		["on_item_erased", removed, "append"]]

	# func_as_args only works when supplied with arrays
	# each item is used as an argument for the referenced function (connect)

	# these all provide the same result
	var r = fdb.qry().as_args(funcref(people, "connect")).apply(connections) # as_args does not execute immediately without to_array()
#	var r = fdb.qry().map(ops.func_as_args(people, "connect")).apply(connections)
#	var r = fdb.for_each(connections, ops.expr('p.connect(_item[0], _item[1], _item[2])', {p=people}))

	# r will contain return values for each item
	print("connect results: %s" % str(r))

	var items = [
		{name="psycheo", age=1, addr_id=1},
		{name="jahne doe", age=199, addr_id=1},
		]

	# calling append, erase will in turn trigger the signals from earlier
#	fdb.mapq(funcref(people, "append")).apply(items)
#	fdb.mapq(funcref(people, "erase")).apply(items)
	fdb.for_each(items, funcref(people, "append"))
	fdb.for_each(items, funcref(people, "erase"))
	# NOTE: it is often quicker to use a for loop
#	for i in items: people.append(i)
#	for i in items: people.erase(i)
	
	print_break_mini()

	print("Items Added:")
	print(added.to_array())
	print("Items Removed:")
	print(removed.to_array())

	fdb.qry().as_args(funcref(people, "disconnect")).apply(connections)
	# no signals anymore
	people.append({name="unknown", age=21, addr_id=1})

func basic():
	print("Some basic ops on ints")
	var a = []
	for i in [1, 2, 3]:
		a.append(i + 2)
		
	printt('+2', a)	
	printt('+2', fdb.for_each([1, 2, 3], '_item+2'))	
	printt('+2', fdb.mapq('_item+2').apply([1, 2, 3]))	
