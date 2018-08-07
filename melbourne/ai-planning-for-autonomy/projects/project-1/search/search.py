# search.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


"""
In search.py, you will implement generic search algorithms which are called by
Pacman agents (in searchAgents.py).
"""

import util

class SearchProblem:
    """
    This class outlines the structure of a search problem, but doesn't implement
    any of the methods (in object-oriented terminology: an abstract class).

    You do not need to change anything in this class, ever.
    """

    def getStartState(self):
        """
        Returns the start state for the search problem.
        """
        util.raiseNotDefined()

    def isGoalState(self, state):
        """
          state: Search state

        Returns True if and only if the state is a valid goal state.
        """
        util.raiseNotDefined()

    def getSuccessors(self, state):
        """
          state: Search state

        For a given state, this should return a list of triples, (successor,
        action, stepCost), where 'successor' is a successor to the current
        state, 'action' is the action required to get there, and 'stepCost' is
        the incremental cost of expanding to that successor.
        """
        util.raiseNotDefined()

    def getCostOfActions(self, actions):
        """
         actions: A list of actions to take

        This method returns the total cost of a particular sequence of actions.
        The sequence must be composed of legal moves.
        """
        util.raiseNotDefined()


def tinyMazeSearch(problem):
    """
    Returns a sequence of moves that solves tinyMaze.  For any other maze, the
    sequence of moves will be incorrect, so only use this for tinyMaze.
    """
    from game import Directions
    s = Directions.SOUTH
    w = Directions.WEST
    return  [s, s, w, s, w, w, s, w]

def depthFirstSearch(problem):
    """
    Search the deepest nodes in the search tree first.

    Your search algorithm needs to return a list of actions that reaches the
    goal. Make sure to implement a graph search algorithm.

    To get started, you might want to try some of these simple commands to
    understand the search problem that is being passed in:
    """
    "*** YOUR CODE HERE ***"

    # inits
    startState = problem.getStartState()
    startNode = Node(startState, None, 0, None)
    frontier = util.Stack()
    frontier.push(startNode)
    visitedNodes = [startNode]
    closedStates = [startState]

    # perform dfs untill goal is found
    goalNode = None
    while(not frontier.isEmpty()):
        node = frontier.pop()
        visitedNodes.append(node)
        closedStates.append(node.state)

        if problem.isGoalState(node.state):
            goalNode = node
            break

        succs = problem.getSuccessors(node.state)
        for succ in succs:
            s, d, c = succ
            if s not in closedStates:
                succNode = Node(s, d, c, node)
                frontier.push(succNode)

    # from goal node, backtrack untill start node
    path = extractPath(goalNode)

    return path

def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"

    # inits
    startState = problem.getStartState()
    completePath = []

    # find path and cost to all goal states



    # for all goal states, find path and cost to all other goal states



    # calculate shortest path between all states

    while True:
        res = performSearch(startState, problem)
        if res is not None:
            path, goalState = res
            if startState == goalState:
                break

            completePath.extend(path)
            startState = goalState
        else:
            break

    return completePath

def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"

    # inits
    startState = problem.getStartState()
    startNode = Node(startState, None, 0, None)
    frontier = util.PriorityQueue()
    frontier.push(startNode, startNode.cost)
    visitedNodes = [startNode]
    closedStates = [startState]

    # perform ucs untill goal is found
    goalNode = None
    while(not frontier.isEmpty()):
        node = frontier.pop()
        visitedNodes.append(node)
        closedStates.append(node.state)

        if problem.isGoalState(node.state):
            goalNode = node
            break

        succs = problem.getSuccessors(node.state)
        for succ in succs:
            s, d, c = succ

            if s not in closedStates:

                # if the successor is already in the frontier do not add it again. Just update parent
                succsInFrontier = filter(lambda t: t[2].state == s, frontier.heap)
                if len(succsInFrontier) > 0:
                    if succsInFrontier[0][2].cost > node.cost + c:
                        succsInFrontier[0][2].direction = d
                        succsInFrontier[0][2].cost = node.cost + c
                        succsInFrontier[0][2].parent = node
                        frontier.update(succsInFrontier[0][2], node.cost + c)
                    continue

                succNode = Node(s, d, node.cost + c, node)
                frontier.push(succNode, node.cost + c)

    # from goal node, backtrack untill start node
    path = extractPath(goalNode)

    return path

def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0

def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"

    # inits
    startState = problem.getStartState()
    startNode = Node(startState, None, 0, None)
    frontier = util.PriorityQueue()
    frontier.push(startNode, 0)
    visitedNodes = [startNode]
    closedStates = [startState]

    # perform ucs untill goal is found
    goalNode = None
    while(not frontier.isEmpty()):
        node = frontier.pop()
        visitedNodes.append(node)
        closedStates.append(node.state)

        # for i in frontier.heap:
        #     print(i[0], i[1], i[2].state, i[2].direction, i[2].cost)

        if problem.isGoalState(node.state):
            goalNode = node
            break

        succs = problem.getSuccessors(node.state)
        for succ in succs:
            s, d, c = succ

            if s not in closedStates:
                succsInFrontier = filter(lambda t: t[2].state == s, frontier.heap)
                if len(succsInFrontier) > 0:
                    # if the successor is already in the frontier do not add it again. Just update parent
                    if succsInFrontier[0][2].cost + heuristic(s, problem) > node.cost + c + heuristic(s, problem):
                        succsInFrontier[0][2].direction = d
                        succsInFrontier[0][2].cost = node.cost + c
                        succsInFrontier[0][2].parent = node
                        frontier.update(succsInFrontier[0][2], node.cost + c + heuristic(s, problem))
                    continue

                succNode = Node(s, d, node.cost + c, node)
                frontier.push(succNode, node.cost + c + heuristic(s, problem))

    # from goal node, backtrack until start node
    path = extractPath(goalNode)

    return path


# helper classes
class Node:
    def __init__(self, state, direction, cost, parent):
        self.state = state
        self.direction = direction
        self.cost = cost
        self.parent = parent

# helper functions
def extractPath(goalNode):
    currentNode = goalNode
    path = []
    while currentNode.parent is not None:
        path.append(currentNode.direction)
        currentNode = currentNode.parent
    path.reverse()
    return path

def performSearch(startState, problem):
    startNode = Node(startState, None, 0, None)
    frontier = util.Queue()
    frontier.push(startNode)
    visitedNodes = [startNode]
    closedStates = [startState]

    # perform bfs until goal is found
    goalNode = None
    while not frontier.isEmpty():
        node = frontier.pop()
        visitedNodes.append(node)
        closedStates.append(node.state)

        if problem.isGoalState(node.state):
            goalNode = node
            break

        succs = problem.getSuccessors(node.state)
        for succ in succs:
            s, d, c = succ

            if s not in closedStates and len(filter(lambda n: n.state == s, frontier.list)) <= 0:
                succNode = Node(s, d, c, node)
                frontier.push(succNode)

    # from goal node, backtrack untill start node
    if goalNode is not None:
        return (extractPath(goalNode), goalNode.state)
    return None

# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch
