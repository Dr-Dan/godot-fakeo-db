tool
extends EditorScript
const GL = GodotLINQ
const GLFac = GodotLinqFactory

"""
TODO:
	change class names in GL to CamelCase?
	add functions:
		count
		select + group by
		add(_if), remove(_if)
			
	method for accessing nested data
	dictionary/database class?
		following same method as iter
	test library - gut??

	using select as where currently
		select should be for projecting data
	select with func
	
	all functions should be able to take object or dictionary as input
	generally move towards objects as opposed to raw dictionaries
		wrap dictionary in default class
	
"""

class Human:
	var name: String
	var age: int
	var addr_id: int
	
	func _init(name, age, addr_id):
		self.name = name
		self.age = age
		self.addr_id = addr_id
		
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
	Human.new("ariel", 65, 3),
	])
	
	var addr_table = IterList.new([
	{addr_id=0, street="vale road", value=20000}, 
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250},
	{addr_id=3, street="diamond mews", value=100000},	
	])
	
	print_break()
	print_lists(name_table, addr_table)
	print_break()
	do_cmp_func_test(name_table)
	print_break()
	do_age_comp_test(name_table)
	print_break()
	do_house_search_test(name_table, addr_table)

func print_break():
	print("\n###############################")

func print_lists(name_table, addr_table):
	print("PEOPLE:")
	print(name_table.to_list())
	print("\n--------------")
	print("ADDRESSES:")	
	print(addr_table.to_list())

func get_age_diff_less_than(data, age):
	return abs(data.target - age) < data.max_dist
	
func do_cmp_func_test(name_table):
	var values = {target=55, max_dist=30}
	var b = GL.cmp_func_args.new(funcref(self, "get_age_diff_less_than"), values)
	var fields = ["name", "age"]
	var result_func_b = name_table.select({age=b}, fields) 
	
	print("abs(%d - age) < %d ?" % [values.target, values.max_dist])
	print(result_func_b.to_list())
	
func do_age_comp_test(name_table):
	var age_comp = GLFac.and_([GLFac.gt(20), GLFac.lt(100)])
	var age_comp_not = GLFac.not_([age_comp])
	
	var results = name_table.select_grouped(
		{
			comp={age=age_comp},
			comp_not={age=age_comp_not}
		}, 
		["addr_id", "name", "age"]
	)
	var result = results.comp
	var result_not = results.comp_not
	
#	var result = name_table.select({age=age_comp}, ["addr_id", "name", "age"])
#	var result_not = name_table.select({age=age_comp_not}, ["addr_id", "name", "age"])
#
	print("age > 20 and age < 100")
	print(result.to_list())
	print("\n--------------")
	print("not (age > 20 and age < 100) == age < 20 or age > 100")
	print(result_not.to_list())
	
func do_house_search_test(name_table, addr_table):
	# first search all houses worth more than 20000
	var house_search = addr_table.where({value=GLFac.gt(20000)})
	# get address ids from results
	var addr_ids = house_search.get_values("addr_id")
	# find all homeowners whose address is in addr_ids. Select name, address fields
#	var homeowners = name_table.select({addr_id=GLFac.in_(addr_ids)}, ["name", "addr_id"])
	var homeowners = name_table.where({addr_id=GLFac.in_(addr_ids)})
	homeowners = homeowners.select({}, ["name", "addr_id"])
	
	print("house value > 20000")
	print(house_search.to_list())
	print("\n--------------")
	
	print("homeowners where house value > 20000 (addr_id in 'addr_ids')")
	print(homeowners.to_list())
