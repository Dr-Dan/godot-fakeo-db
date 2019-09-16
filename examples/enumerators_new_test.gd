tool
extends EditorScript

# TODO: rename or remove these
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
	
	var test_data = IterListApplier.new(data)
	
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
		.project(["name", "id", "subtype"])
		
	for e in i:
		print(e)
	
var query = QueryBuilder_new.new()\
	.where({dmg=OpFac.gteq(10)})\
	.take(3)\
	.project(["name", "id", "subtype"])
		
func test_query():
	print_break()
	print("QUERY\n")
	for e in query.eval(data):
		print(e)
	
func test_enumerators():
	print_break()
	print("ENUMERATOR\n")
	var E = Enumerators_new.Where.new({dmg=OpFac.gteq(10)}, IterList.new(data))
	var P = Enumerators_new.Project.new(["name", "id", "subtype"], E)
	var T = Enumerators_new.Take.new(3, P)
	var TE = Enumerators_new.Where.new({
		subtype=OpFac.or_([OpFac.eq("spear"), OpFac.eq("blunt")])}, T)

	for e in E:
		print(e)
	print_break()

	for e in P:
		print(e)
	print_break()

	for e in T:
		print(e)
	print_break()

	for e in TE:
		print(e)
	print_break()
	
# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")