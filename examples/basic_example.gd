tool
extends EditorScript

# TODO: rename or remove these
const GL = Comparers
const GLF = Factory

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
	run_test()
	
func run_test():
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
	{addr_id=3, street="diamond mews", value=100000},
	Address.new(3, "diamond mews", 100000)
	]
	
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
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")


func print_lists(name_table, addr_table):
	var query_names = Chainer.new()\
	.select(["name", "age", "addr_id"])
	var query_addr = Chainer.new()\
	.select(["addr_id", "street", "value"])

	print("PEOPLE:")
	print(query_names.eval(name_table))
	print("\n--------------")
	print("ADDRESSES:")
	print(query_addr.eval(addr_table))

# ==============================================================

# store a query for later
var age_comp = GLF.and_([GLF.gt(20), GLF.lt(70)])
var age_comp_not = GLF.not_(age_comp)
var fields = ["addr_id", "name", "age"]

var where_age = Chainer.new()\
	.where({age=age_comp})\
	.select(fields)

var where_not_age = Chainer.new()\
	.where({age=age_comp_not})\
	.select(fields)

func age_comp_builder_test(name_table):
	var result = where_age.eval(name_table)
	var result_not = where_not_age.eval(name_table)

	print("age > 20 and age < 70")
	print(result)
	print_break_mini()
	print("not (age > 20 and age < 70)")
	print(result_not)

# ==============================================================

func house_search_test(name_table, addr_table):
	var query_addr_value = Chainer.new()\
		.where({value=GLF.gt(20000)})\
		.select(["addr_id", "street", "value"])

	var valued_houses = query_addr_value.eval(addr_table)

	var addr_ids = Chainer.new()\
		.values("addr_id")\
		.eval(valued_houses)

	var query_homeowner = Chainer.new()\
		.where({addr_id=GLF.in_(addr_ids)})\
		.select(["name", "addr_id"])
	var homeowners = query_homeowner.eval(name_table)

	print("house value > 20000")
	print(valued_houses)
	print_break_mini()
	print("homeowners where house value > 20000 (addr_id in 'addr_ids')")
	print(homeowners)
	
# ==============================================================

func i_is_in_name(name:String):
	return "i" in name

func count_names_test(name_table):
	var cmp_has_i = GL.CmpFunction.new(funcref(self, "i_is_in_name"))
	var cmp_is_dan = {name=GLF.eq("dan")}

	var result_name = Chainer.new()\
	.count({name=cmp_has_i})\
	.eval(name_table)

	var result_age = Chainer.new()\
	.first(cmp_is_dan)\
	.eval(name_table)

	print("%d/%d name entries contain 'i'" % [result_name, name_table.size()])
	print("dan's age is %d" % result_age.age)

# ==============================================================

func name_starts_with_letter(name:String, letter:String):
	return not name.empty() and name[0].to_lower() == letter

func take_test(name_table, amt_take=2):
	var cmp = GL.CmpFunctionWithArgs.new(funcref(self, "name_starts_with_letter"), "a")

	var query = Chainer.new()\
	.where({name=cmp})\
	.take(amt_take)\
	.select(["name", "age"])

	var result = query.eval(name_table)
	print("take %d entries where name starts with 'a':" % amt_take)
	print(result)

