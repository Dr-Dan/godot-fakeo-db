tool
extends EditorScript
const GL = Comparers
const GLF = Factory

"""
To use: File > Run
"""

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
			
func _run():
	run_test()
	

# Called when the node enters the scene tree for the first time.
func run_test():
	var name_table = IterList.new([
	{name="mike", age=22, addr_id=0},
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=49, addr_id=2},
	{name="aslan", age=199, addr_id=1},
	Human.new("dan", 30, 2),
	Human.new("andy", 76, 2),
	Human.new("ariel", 65, 3),
	])
	
	var addr_table = IterList.new([
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250},
	{addr_id=3, street="diamond mews", value=100000},
	Address.new(3, "diamond mews", 100000)
	])
	
	print_break()
	print_lists(name_table, addr_table)
	print_break()
	take_test(name_table)
	print_break()
	count_names_test(name_table)
	print_break()
	age_comp_builder_test(name_table)
	print_break()
	house_search_test(name_table, addr_table)

func print_break():
	print("\n###############################")

func print_lists(name_table, addr_table):
	print("PEOPLE:")
	print(name_table.to_list())
	print("\n--------------")
	print("ADDRESSES:")
	print(addr_table.to_list())

func i_is_in_name(name:String):
	return "i" in name
	
func count_names_test(name_table):
	var cmp = GL.CmpFunction.new(funcref(self, "i_is_in_name"))
	var result = name_table.count({name=cmp})
	print("%d/%d name entries contain 'i'" % [result, name_table.size()])
	
	var comp_dan = {name=GLF.eq("dan")}
	print("dan's age is %d" % name_table.first(comp_dan).age)

func name_starts_with_letter(name:String, letter:String):
	return not name.empty() and name[0].to_lower() == letter
	
func take_test(name_table):
	var cmp = GL.CmpFunctionWithArgs.new(funcref(self, "name_starts_with_letter"), "a")
	var result = name_table.take({name=cmp}, 2).select(["name", "age"])
	print("first two where name starts with 'a':")
	print(result.to_list())
	
# store query for later
var age_comp = GLF.and_([GLF.gt(20), GLF.lt(100)])
var age_comp_not = GLF.not_(age_comp)
var fields = ["addr_id", "name", "age"]

var chain = Builders.Chainer.start()\
	.where({age=age_comp})\
	.select(fields)

var chain_not = Builders.Chainer.start()\
	.where({age=age_comp_not})\
	.select(fields)

func age_comp_builder_test(name_table):
	var result = chain.eval(name_table)
	var result_not = chain_not.eval(name_table)

	print("age > 20 and age < 100")
	print(result.to_list())
	print("\n--------------")
	print("not (age > 20 and age < 100) == age < 20 or age > 100")
	print(result_not.to_list())
	
func house_search_test(name_table, addr_table):
	var house_search = addr_table.where({value=GLF.gt(20000)})
	# get address ids from results
	var addr_ids = house_search.get_values("addr_id")
	# find all homeowners with address in addr_ids. Select name, address fields
	var homeowners = name_table\
	.where({addr_id=GLF.in_(addr_ids)})\
	.select(["name", "addr_id"])
	
	print("house value > 20000")
	print(house_search.to_list())
	print("\n--------------")
	
	print("homeowners where house value > 20000 (addr_id in 'addr_ids')")
	print(homeowners.to_list())