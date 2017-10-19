import lparser
import smcparser
import node as nd
import smc
import esmc

def run():
    # run_smc()
    run_lparser()

def run_smc():
	calc = smcparser.Calc()
	tree = calc.run()
	nd.print_tree(tree)
	machine = smc.SMCmachine()
	a = machine.load_program(tree)
	machine.print_state()
	machine.run_program()

def run_lparser():
	calc = lparser.Calc()
	tree = calc.run()
	nd.print_tree(tree)
	machine = esmc.SMCmachine()
	a = machine.load_program(tree)
	machine.print_state()
	machine.run_program()
    


if __name__ == '__main__':
    run()