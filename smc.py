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
         self.e = Stack()
         self.debug = False

	def print_state(self):
		if(self.debug == True):
			state = '<'+ str(self.e.items) + 'E,  '+ str(self.s.items) + 'S,  ' + str(self.m) + 'M,  ' + str(self.c.items) + 'C>'
			print state

	def load_and_run(self, current_node):
		if(isinstance(current_node,nd.Node)):
			if(current_node.type == 'binop'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[1])
				self.load_and_run(current_node.children[0]) 					 	
				self.c.pop()
				self.evaluate_binary_op(current_node.leaf[0])
				self.print_state()

			elif(current_node.type == 'separator'):
				self.load_and_run(current_node.children[0])
				self.load_and_run(current_node.children[1])
				self.print_state()

			elif(current_node.type == 'assign' or current_node.type == 'boolassign'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0])				
				self.load_and_run(current_node.children[1])


				self.c.pop()
				value = self.s.pop()
				variable = self.s.pop()
				enviromentPos = self.enviromentSearch(variable)

				if(enviromentPos == -1):
					raise Exception("variable '"+str(variable)+"' not found")
				elif(isinstance(self.e.items[enviromentPos],EnviromentConst)):
					raise Exception(variable +" is a const")
				else:
					memAddress = self.e.items[enviromentPos]
					memCell = self.m[memAddress.address]
					if( type(memCell.data) != type(value) ):
						raise Exception("trying to store "+ type(value).__name__ +" value in " + type(memCell.data).__name__+ " variable" )
					else:
						memCell.data = value
				
				
				self.print_state()

			elif(current_node.type == 'booleanexpression_binop'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0]) 	
				self.load_and_run(current_node.children[1]) 	
				self.c.pop()				
				self.evaluate_binary_bool(current_node.leaf)
				self.print_state()

			elif(current_node.type == 'booleanexpressionnot'):
				self.c.push(current_node)
				child0 = self.load_and_run(current_node.children[0])				
				value = not self.s.pop()
				self.s.push(value)
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'conditional' or current_node.type == 'expression_conditional'):

				self.c.push(current_node)

				self.load_and_run(current_node.children[0])

				booleanblockResult = self.s.pop()
				self.print_state()

				
				if(booleanblockResult == True):
					self.load_and_run(current_node.children[1])
				else:
					self.load_and_run(current_node.children[2])	
				
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'loop'):

				self.c.push(current_node)

				self.load_and_run(current_node.children[0])
				booleanblockResult = self.s.pop()
				self.print_state()

				while(booleanblockResult == True):
					self.load_and_run(current_node.children[1])

					self.load_and_run(current_node.children[0])
					booleanblockResult = self.s.pop()
					self.print_state()

				self.c.pop()
				self.print_state()

			elif(current_node.type == 'command_block'):
				self.c.push(current_node)				

				actualEnviroment = list(self.e.items)

				self.load_and_run(current_node.children[0])

				self.c.pop()
				self.e.items = actualEnviroment
				self.print_state()
				self.freeMemory()
				self.print_state()

			elif(current_node.type == 'booleanexpression_block'):
				self.load_and_run(current_node.children[0])
				

			elif(current_node.type == 'declaration_const'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[2])								
				value = self.s.pop() #get a value from values stack
				constName = current_node.children[1]
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeConstOnEnviroment(EnviromentConst(constName,value))					
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.c.pop()
				self.print_state()		

			elif(current_node.type == 'declaration_var'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[2])	
				value = self.s.pop() #get a value from values stack							
				varName = current_node.children[1]
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeVarOnMemory(varName,value)
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.c.pop()
				self.print_state()
			
			elif(current_node.type == 'declaration_procedure'):
				self.c.push(current_node)
				parameterList = []
				parameterList = self.loadParameterList(current_node.children[1])				
				procedure = ProcedureLink(current_node.children[0],parameterList,current_node.children[2])
				self.e.push(procedure)
				self.c.pop()
				self.print_state()


			elif(current_node.type == 'procedure_call'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[1])

				procedure = self.findProcedure(current_node.children[0])
				if(procedure == None):
					raise Exception("Procedure " + current_node.children[0] + " not found")
				else:
					actualEnviroment = list(self.e.items)
					parameterList = procedure.parameterList
					#cria novas variaveis
					for parameter in reversed(parameterList):						
						value = self.s.pop()
						if(isinstance(value,str)):
							self.loadFromMemoryToS(value)
							value = self.s.pop()
						node = nd.Node("declaration_var", [parameter.children[0],parameter.children[2],value], ['var','='])
						self.load_and_run(node)

					self.load_and_run(procedure.commandBlock)

					self.c.pop()
					self.e.items = actualEnviroment
					self.print_state()
					self.freeMemory()
					self.print_state()

			elif(current_node.type == 'function_call'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[1])

				procedure = self.findProcedure(current_node.children[0])
				if(procedure == None):
					raise Exception("Function " + current_node.children[0] + " not found")
				else:
					actualEnviroment = list(self.e.items)
					parameterList = procedure.parameterList
					#cria novas variaveis
					for parameter in reversed(parameterList):						
						value = self.s.pop()
						if(isinstance(value,str)):
							self.loadFromMemoryToS(value)
							value = self.s.pop()
						node = nd.Node("declaration_var", [parameter.children[0],parameter.children[2],value], ['var','='])
						self.load_and_run(node)

					self.load_and_run(procedure.commandBlock)

					self.c.pop()
					self.e.items = actualEnviroment
					self.print_state()
					self.freeMemory()
					self.print_state()

			elif(current_node.type == 'chain_parameter'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[0])
				self.load_and_run(current_node.children[1])
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'single_parameter'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[0])
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'print'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0])
				value = self.s.pop()

				if(isinstance(value,str)):
					self.c.push(value)
					self.loadFromMemoryToS(value)
					self.c.pop()
					self.print_state()
					value = self.s.pop()
					
				print "output: " + str(value)
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'declaration'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0])
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'return'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0])
				value = self.s.pop()
				if(isinstance(value,str)):
					self.loadFromMemoryToS(value)
				else:
					self.s.push(value)
				self.c.pop()
				self.print_state()


				
		elif(current_node == 'true' or current_node == 'false'):
			value = (current_node == 'true')
			self.s.push(value)
			self.print_state()	

		else:
			self.s.push(current_node)
			self.print_state()

	def loadParameterList(self,parameterListNode):
		numOfParams = self.travelParameterList(parameterListNode)
		parameterList = []
		for i in range (0, numOfParams):
			parameterList.append(self.c.pop())
		return parameterList

	def travelParameterList(self,parameterListNode):
		if(parameterListNode.type == 'declaration_parameter'):
			self.c.push(parameterListNode)
			return 1
		elif(parameterListNode.type == 'declaration_parameterList'):
			numA = self.travelParameterList(parameterListNode.children[0]);
			numB = self.travelParameterList(parameterListNode.children[1]);
			return numA+numB
		elif(parameterListNode.type == 'declaration_parameterHead'):
			numA = self.travelParameterList(parameterListNode.children[0]);
			return numA

	def findProcedure(self, procedureName):
		enviroment = self.e.items
		index = len(self.e.items) - 1		
		for item in reversed(self.e.items):			
			if(isinstance(item,ProcedureLink)):
				if(item.procedureName == procedureName):
					return item
		return None


	
	def eval_command_block(self, commandblock):

		actualControlStack = list(self.c.items)
		self.c.items = commandblock.command.items
		#print "command_block" + str(actualControlStack)

		actualEnviroment = list(self.e.items)

		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		self.c.items = actualControlStack
		self.e.items = actualEnviroment



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

	def enviromentSearch(self,varName):
		index = len(self.e.items) - 1		
		for item in reversed(self.e.items):			
			if(isinstance(item,EnviromentConst)):
				if(item.name == varName):
					return index
			elif(isinstance(item,MemoryAddress)):
				memoryCell = self.m[item.address]
				if(memoryCell.variableName == varName):
					return index
			index-=1

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
		if(operator == 'add'):
			self.s.push(op1+op2);
		if(operator == 'sub'):
			self.s.push(op1-op2);
		if(operator == 'mul'):
			self.s.push(op1*op2);
		if(operator == 'div'):
			self.s.push(op1/op2);
		self.print_state();

	def evaluate_binary_bool(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()

		if(not isinstance(op1,bool) and not isinstance(op1,numbers.Number)):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();
		if(not isinstance(op2,bool) and not isinstance(op2,numbers.Number)):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();		

		if(operator == "eq"):
			if(isinstance(op1,numbers.Number) and isinstance(op2,numbers.Number)):
				self.s.push(op1 == op2)			
			else:
				raise Exception("impossible to compare values "+ str(op1) + " and " + str(op2))

		else:			
			if(operator == 'and'):
				self.s.push(op1 and op2)
			if(operator == 'or'):
				self.s.push(op1 or op2)				
		
		self.print_state();

	def loadFromMemoryToS(self,varName):
		address = self.enviromentSearch(varName)

		if(address == -1):
			raise Exception("var '" +str(varName) +"' not found")
		else:
			enviromentItem = self.e.items[address]
			if(isinstance(enviromentItem,EnviromentConst)):
				data = enviromentItem.value
				self.s.push(data)
			elif(isinstance(enviromentItem,MemoryAddress)):
				memoryCell = self.m[enviromentItem.address]
				self.s.push(memoryCell.data)
			self.print_state()

	def storeConstOnEnviroment(self,enviromentConst):
		self.e.push(enviromentConst)

	def storeVarOnMemory(self,varName,varValue):
		
		self.m.append(MemoryCell(varValue,varName))
		self.e.push(MemoryAddress(type(varValue),len(self.m) - 1))			
				

	def loadFromEnviroment(self, varName):

		for envItem in reversed(self.e.items):
			if(isinstance(envItem,EnviromentConst) and envItem.name == varName):
				self.s.push(envItem.value)
			elif(isinstance(envItem,MemoryAddress)):
				memoryCell = self.m[envItem.address]
				if(memoryCell.variableName == varName):
					self.s.push(memoryCell.data)			

	def freeMemory(self):
		itemFound = False

		for idx in range(len(self.m)):
			memItem = self.m[idx]

			itemFound = False
			for envItem in self.e.items:				
				if(isinstance(envItem,MemoryAddress)):
					memoryCell = self.m[envItem.address]
					if(memItem == memoryCell):						
						itemFound = True
						break

			if(itemFound == False):				
				self.m[idx] = None

		self.m = filter(None,self.m)


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
	def __init__(self, varType, memoryReference):
		self.type = varType	
		self.address = memoryReference	

	def __repr__(self):
		return "MemAddr("+self.type.__name__+"|"+str(self.address)+")"

class EnviromentConst:
	def __init__(self, name,value):
		self.name = name
		self.value = value	
		self.type = type(value).__name__

	def __repr__(self):
		return "EnvConst("+self.type+" "+self.name+"="+str(self.value)+")"

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

class ProcedureLink:
	def __init__(self, procedureName,parameterList, commandBlock):
		self.procedureName = procedureName
		self.parameterList = parameterList
		self.commandBlock = commandBlock

	def __repr__(self):
		return "procLink(" +self.procedureName+ ")"


