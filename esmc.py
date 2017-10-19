from __future__ import division
import ply.lex as lex
import ply.yacc as yacc
import os
import numbers
import node as nd

class SMCmachine:
	def __init__(self):         
         self.s = Stack()
         self.m = []
         self.c = Stack()
         self.debug = True

	def print_state(self):
		if(self.debug == True):
			state = '<'+ str(self.s.items) + 'S,' + str(self.m) + 'M,' + str(self.c.items) + 'C>'
			print state




	def load_program(self, current_node):

		if(isinstance(current_node,nd.Node)):
			if(current_node.type == 'binop'):
				self.c.push(current_node.leaf[0])
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()

			elif(current_node.type == 'separator'):
				
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.c.push(current_node.leaf[0])

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)

				self.print_state()

			elif(current_node.type == 'assign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()

			elif(current_node.type == 'booleanexpression_binop'):
				self.c.push(str(current_node.leaf))
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()
			
			elif(current_node.type == 'boolassign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()
			elif(current_node.type == 'booleanvalues'):
				self.c.push(current_node.leaf)
				
				self.print_state()
			
			elif(current_node.type == 'booleanexpressionnot'):
				self.c.push(str(current_node.leaf))
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				
				
				self.print_state()

			elif(current_node.type == 'loop'):

				
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)

				self.c.push(current_node.leaf[1])	
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(current_node.leaf[0])	
				self.print_state()


			elif(current_node.type == 'conditional'):

				child2 = self.load_program(current_node.children[2])
				if(child2 is not None):			
					self.c.push(child2)
				self.c.push(current_node.leaf[2])

				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)

				self.c.push(current_node.leaf[1])	
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(current_node.leaf[0])	
				self.print_state()	

			elif(current_node.type == 'command_block' or current_node.type == 'booleanexpression_block'):
				blockContent = Stack()				
				self.c.push(current_node.leaf[0])
				first_par_pos = self.c.size() - 1
				child0 = self.load_program(current_node.children[0]) 										
				self.c.push(current_node.leaf[1])				
				last_par_pos = self.c.size() - 1
				self.print_state()

				block_elements_list = self.c.items[ (first_par_pos+1) : (last_par_pos) ]   
				blockContent.items = block_elements_list
				

				del self.c.items[first_par_pos:last_par_pos+1]

				if(current_node.type == 'command_block'):
					commandBlock = Command(blockContent)
					self.c.push(commandBlock)
				else:
					commandBlock = BooleanBlock(blockContent)
					self.c.push(commandBlock)

							

				self.print_state()

		else:
			return current_node		


	def run_program(self):

		print "\n running program \n"
		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		
		self.print_state();


	def eval_control(self, control):
		if(isinstance(control,numbers.Number)):
				self.s.push(control)
				self.print_state();

		elif(isinstance(control,str)):
			if(control == '+' or control == '/' or control == '*' or control == '-'):
				self.evaluate_binary_op(control)	
				
			elif(control == 'and' or control == 'or' or control == 'eq'):
				self.evaluate_binary_bool(control)	
				
			elif(control=='tt' or control=='ff'):
				value = self.s.push(control);					
				self.print_state();

			elif(control =='not'):
				value = self.s.pop();
				if(value == 'tt'):
					self.s.push('ff')
				else:
					self.s.push('tt')


			elif(control=='while'):
				self.print_state();
				booleanblock = self.c.pop()

				
				boolstack = Stack()
				boolstack.items = list(booleanblock.content.items)
				booleanblock_copy = BooleanBlock(boolstack)
				self.eval_boolean_block(booleanblock_copy)

				self.print_state();
				eval_result = self.s.pop()
				do_word = self.c.pop()
				self.print_state();

				commandblock = self.c.pop()

				cmdstack = Stack()
				cmdstack.items = list(commandblock.command.items)
				commandblock_copy = Command(cmdstack)	


				if(eval_result == 'tt'):
					self.eval_command_block(commandblock_copy)

					self.c.push(commandblock)	
					self.c.push(do_word)
					self.c.push(booleanblock)
					self.c.push(control)
					self.print_state()

			elif(control == 'if'):
				booleanblock = self.c.pop()
				self.print_state()
				self.eval_boolean_block(booleanblock)

				self.print_state()
				eval_result = self.s.pop()
				self.print_state()
				word_then = self.c.pop()
				self.print_state()

				commandblock = self.c.pop()				
				self.print_state()
				word_else = self.c.pop()
				self.print_state()
				elsecommandblock = self.c.pop()
				self.print_state()

				if(eval_result == 'tt'):
					self.eval_command_block(commandblock)
				else:
					self.eval_command_block(elsecommandblock)
					
				self.print_state()

					
			elif(control == ':='):
				variable = self.c.pop();
				value = self.s.pop();
				memoryPos = self.var_exists(variable)

				if(memoryPos == -1):
					memoryCell = MemoryCell(value,variable)
					self.m.append(memoryCell)
					#memAddress = MemoryAddress(len(self.m) - 1)
					#self.s.push(memAddress)
					#self.s.push(variable)
				else:
					self.m[memoryPos].data = value
					##memAddress = MemoryAddress(memoryPos)
					#self.s.push(memAddress)
			elif(control == ';'):
				self.print_state()
			else:
				self.s.push(control)
				self.print_state();
	
	def eval_command_block(self, commandblock):
		self.debug = False

		actualControlStack = list(self.c.items)
		self.c.items = commandblock.command.items
		#print "command_block" + str(actualControlStack)

		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		self.c.items = actualControlStack

		self.debug = True

	def eval_boolean_block(self, booleanblock):
		self.debug = False

		actualControlStack = list(self.c.items)
		self.c.items = booleanblock.content.items
		#print "boolean_block" + str(actualControlStack)

		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		self.c.items = actualControlStack

		self.debug = True

	def var_exists(self,varName):
		index = 0
		for cell in self.m:
			if(varName == cell.variableName):
				return index
			index+=1

		return -1

	def evaluate_binary_op(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()		
		if(isinstance(op1,str)):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();		
		if(isinstance(op2,str)):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();
		
		self.print_state()
		if(operator == '+'):
			self.s.push(op1+op2);
		if(operator == '-'):
			self.s.push(op1-op2);
		if(operator == '*'):
			self.s.push(op1*op2);
		if(operator == '/'):
			self.s.push(op1/op2);
		self.print_state();

	def evaluate_binary_bool(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()

		if(operator == "eq"):		
			if(isinstance(op1,str)):
				self.loadFromMemoryToS(op1)
				op1 = self.s.pop();		
			if(isinstance(op2,str)):
				self.loadFromMemoryToS(op2)
				op2 = self.s.pop();

			if(op1 == op2):
				self.s.push('tt')
			else:
				self.s.push('ff')

		else:
			if(isinstance(op1,str) and not (op1 == 'tt' or op1 == 'ff')):
				self.loadFromMemoryToS(op1)
				op1 = self.s.pop();		
			if(isinstance(op2,str)and not (op2 == 'tt' or op2 == 'ff')):
				self.loadFromMemoryToS(op2)
				op2 = self.s.pop();
			temp1 = op1 == 'tt'
			temp2 = op2 == 'tt'
			self.print_state()
			if(operator == 'and'):
				if(temp1 and temp2):
					self.s.push('tt')
				else:
					self.s.push('ff')
			if(operator == 'or'):
				if(temp1 or temp2):
					self.s.push('tt')
				else:
					self.s.push('ff')
		
		self.print_state();

	def loadFromMemoryToS(self,varName):
		address = self.var_exists(varName)
		memCell = self.m[address]
		self.s.push(memCell.data)
		self.print_state()

class Stack:
     def __init__(self):
         self.items = []

     def isEmpty(self):
         return self.items == []

     def push(self, item):
         self.items.append(item)

     def pop(self):
         return self.items.pop()

     def peek(self):
         return self.items[len(self.items)-1]

     def size(self):
         return len(self.items)

class MemoryCell:
	def __init__(self,data, var):
		self.data = data
		self.variableName = var
	def __repr__(self):
		return "M["+str(self.data)+"/"+str(self.variableName)+"]"


class MemoryAddress:
	def __init__(self, address):
		self.address = address	

	def __repr__(self):
		return "M("+str(self.address)+")"

class Command:
	def __init__(self, commandlist):
		self.command = commandlist
	def __repr__(self):
		return "c("+str(self.command.items)+")"

class BooleanBlock:
	def __init__(self, booleanBlockcontent):
		self.content = booleanBlockcontent
	def __repr__(self):
		return "c("+str(self.content.items)+")"

class Value:
	def __init__(self, commandstring):
		self.command = comandstring
	def __repr__(self):
		return "v("+str(self.command)+")"


