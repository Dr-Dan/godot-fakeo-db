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
			
	print_break()
	test_dmg_where(test_data)
	print_break()
	take_n_test(test_data, "Bow")
	
func test_dmg_where(data):
	var query_melee_dmg = [
		Enumerators.Where.new({
			type=OpFac.eq("melee"), 
			subtype=OpFac.or_([
				OpFac.eq("sword"), 
				OpFac.eq("spear")
			])
		}),
		Enumerators.Select.new(["name", "dmg"])
	]
	var result = data.apply(query_melee_dmg)
	
	print("melee weapons (sword, spear): dmg and name\n")	
	print(result.items)
	print_break_mini()
	
	var query = [
	Enumerators.Where.new({dmg=OpFac.gteq(10)}),
	Enumerators.Select.new(["name", "dmg", "range", "firing_rate"])
	]
	var result1 = data.apply(query)
		
	print("weapons where dmg >= 10\n")
	for i in result1:
		print(i)

func name_contains_word(name:String, letter:String):
	return not name.empty() and letter in name

func take_n_test(data, word, amt_take=2):
	var cmp = Op.CmpFunctionWithArgs.new(
		funcref(self, "name_contains_word"), word)
	var query = [
		Enumerators.Where.new({name=cmp}), 
		Enumerators.Take.new(2)
	]
	var result = data.apply(query)
	
	print("first 2 results containing [%s]\n" % word)
	for i in result:
		print(i)

# ==============================================================
func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")

