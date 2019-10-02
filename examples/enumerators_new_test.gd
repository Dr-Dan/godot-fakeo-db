tool
extends EditorScript


const Op = Operators
const OpFac = OperatorFactory

var data = [
	{name="Wooden Sword", type="melee", subtype="sword", dmg=2, range=1.2},
	{name="Katana", type="melee", subtype="sword", dmg=16, range=1.6},
	{name="Nice Spear", type="melee", subtype="spear", dmg=13, range=2.0},
	{name="Jan's Hammer", type="melee", subtype="blunt", dmg=30, range=1.3},
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, range=20.0, firing_rate=1.4},
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, range=30.0, firing_rate=1.0},
]

func _run():
	for i in range(data.size()):
		data[i]["id"] = i
	
	var test_data = IterList_new.new(data)
	
	print_break()
	print("WEAPON DATA\n")
	for d in test_data:
		print(d)
		
	test_iterlist()
	test_query()
	test_enumerators()

func test_iterlist():
	print_break()
	print("ITERLIST\n")
	var i = IterList_new.new(data)\
		.where({dmg=OpFac.gteq(10)})\
		.take(3)\
		.project(["name", "id", "dmg"])
		
	for e in i:
		print(e)
	
var query = QueryBuilder_new.new()\
	.where({dmg=OpFac.gteq(10)})\
	.take(3)\
	.project(["name", "id", "subtype"])\
		
func test_query():
	print_break()
	print("QUERY\n")
	var result = query.eval(data)
	for e in result:
		print(e)
		
	print_break_mini()
	print(result.first({}).value())
	
func test_enumerators():
	print_break()
	print("ENUMERATOR\n")
	var where = Enumerators_new.Where.new({dmg=OpFac.gteq(10)}, IterList.new(data))
	var project = Enumerators_new.Project.new(["name", "id", "subtype"], where)
	var take = Enumerators_new.Take.new(3, project)
	var first = Enumerators_new.First.new({dmg=OpFac.gteq(20)}, where)

	print(first.value())
		
	print_break_mini()

	for e in where:
		print(e)

	print_break_mini()
	for e in project:
		print(e)

	print_break_mini()
	for e in take:
		print(e)
		
	print_break()

# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")