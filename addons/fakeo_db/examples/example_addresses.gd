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
	var name_table = fdb.cltn([
	{name="mike", age=22, addr_id=0},
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=49, addr_id=2},
	{name="aslan", age=199, addr_id=1},
	Human.new("dan", 30, 2),
	Human.new("andy", 76, 2),
	Human.new("ariel", 65, 3),
	])

	var addr_table = fdb.cltn([
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250},	
	Address.new(3, "diamond mews", 100000)
	])

	print_break()
	collection_append_remove(name_table)
	
	print_break()
	print_lists(name_table, addr_table)
	
	print_break()
	age_comp_builder_test(name_table)
	
	print_break()
	house_search_test(name_table, addr_table)
	
	print_break()
	count_names_test(name_table)
	
	print_break()
	take_test(name_table)

# ==============================================================


func collection_append_remove(name_table):
	var removed = fdb.cltn()
	var added = fdb.cltn()
	
	var connections = [
		# call function or append to another collection when modified
		["on_item_added", self, "_on_item_added"],
		["on_item_erased", self, "_on_item_erased"],

		["on_item_added", added, "append"],
		["on_item_erased", removed, "append"]]
		
	for c in connections:
		name_table.connect(c[0], c[1], c[2])
		
	name_table.append({name="aslan", age=199, addr_id=1})
	var item = {name="fakeo", age=1, addr_id=1}
	name_table.append(item)
	name_table.erase(item)
	
	print_break_mini()
	
	print("Items Removed:")
	print(removed.to_array())
	print("Items Added:")
	print(added.to_array())
	
	for c in connections:
		name_table.disconnect(c[0], c[1], c[2])
		
func _on_item_added(item):
	print("added " + str(item))
	
func _on_item_erased(item):
	print("removed " + str(item))

# ==============================================================

func print_lists(name_table, addr_table):
	var query_names = name_table\
	.project(["name", "age", "addr_id"])
	
	var query_addr = addr_table\
	.project(["addr_id", "street", "value"])

	print("PEOPLE:")
	for i in query_names: print(i)
	print("\n--------------")
	print("ADDRESSES:")
	for i in query_addr: print(i)

# ==============================================================

# store a query for later use
# execution is deferred until to_array() is called later
var age_comp = ops.and_([ops.gt(20), ops.lt(70)]) # 20 < age < 70
var age_comp_not = ops.not_(age_comp)
var fields = ["name", "age"]

var where_age = fdb.QueryBuilder.new()\
	.where({age=age_comp})\
	.project(fields)

var where_not_age = fdb.QueryBuilder.new()\
	.where({age=age_comp_not})\
	.project(fields)

func age_comp_builder_test(name_table):
	""" 
	no actual iteration has taken place yet
		 eval() sets the root array of the query and could also be
		 called outside the function.
	"""
	var result = where_age.eval(name_table)
	var result_not = where_not_age.eval(name_table)

	# to_array() has the added benefit of returning dictionaries which print nicely
	# changes to the result (of result.to_array()) will not affect the original container
	print("age > 20 and age < 70")
	print(result.to_array())
	print_break_mini()
	print("not (age > 20 and age < 70)")
	print(result_not.to_array())

# ==============================================================

func get_addr(item):
	return item.addr_id
	
func house_search_test(name_table, addr_table):
	var valued_houses = addr_table\
		.where({value=ops.gteq(20000)})\
		.project(["addr_id", "street", "value"])

	var addr_ids = valued_houses\
		.select(self, "get_addr")

	var homeowners = name_table\
		.where({addr_id=ops.in_(addr_ids)})\
		.project(["name", "addr_id"])

	print("house value > 20000")
	print(valued_houses.to_array())
	print_break_mini()
	print("homeowners where house value > 20000 (addr_id in 'addr_ids')")
	print(homeowners.to_array())
	
# ==============================================================

func i_is_in_name(name:String):
	return "i" in name

func count_names_test(name_table):
	var cmp_has_i = ops.func_op(self, "i_is_in_name")
	# it is fine to simply state the value if equality is desired
	var cmp_is_dan = {name="dan"}
	
	var result_name = name_table\
	.where({name=cmp_has_i})\
	.count()

	var result_age = name_table\
	.where(cmp_is_dan)\
	.at(0)

	print("%d/%d name entries contain 'i'" % [result_name, name_table.count()])
	print("dan's age is %d" % result_age.age)

# ==============================================================

func name_starts_with_letter(name:String, letter:String):
	return not name.empty() and name[0].to_lower() == letter

func take_test(name_table, amt_take=2):
	var cmp = ops.func_op_args(self, "name_starts_with_letter", ["a"])

	var result = name_table\
	.where({name=cmp})\
	.take(amt_take)\
	.project(["name"])

	print("take %d entries where name starts with 'a':" % amt_take)
	print(result.to_array())
	
# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")

# ==============================================================
