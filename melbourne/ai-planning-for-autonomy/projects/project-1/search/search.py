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

def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0


# class to keep track of current state, the path taken to get here and the cost of getting there
class Node:
    def __init__(self, state, path, cost):
        self.state = state
        self.path = path
        self.cost = cost

# function that takes in a problem, a data structure for keeping track of next nodes to be search, the type of search and maybe a heuristic
def search(problem, frontier, type, heuristic=nullHeuristic):
    closedStates = []
    startState = problem.getStartState()

    if type == "dfs" or type == "bfs":
        frontier.push(Node(startState, [], 0))
    else:
        frontier.push(Node(startState, [], 0), heuristic(startState, problem))

    # until solution is found, go through frontier, adding states to closed and new states to frontier
    while frontier:
        node = frontier.pop()

        if not node.state in closedStates:
            closedStates.append(node.state)
            if problem.isGoalState(node.state):
                return node.path

            # If nodes state is not a goal state, continue expanding the frontier with its successors
            succs = problem.getSuccessors(node.state)
            for successor in succs:
                s, d, c = successor
                newPath = node.path + [d]

                if type == "dfs" or type == "bfs":
                    frontier.push(Node(s, newPath, c))
                else:
                    frontier.push(Node(s, newPath, node.cost + c), node.cost + c + heuristic(s, problem))

    # no solution could be found
    return []




def depthFirstSearch(problem):
    """
    Search the deepest nodes in the search tree first.

    Your search algorithm needs to return a list of actions that reaches the
    goal. Make sure to implement a graph search algorithm.

    To get started, you might want to try some of these simple commands to
    understand the search problem that is being passed in:
    """
    "*** YOUR CODE HERE ***"
    closedStates = []
    startState = problem.getStartState()
    frontier = util.Stack()
    frontier.push(Node(startState, [], 0))

    # until solution is found, go through frontier, adding states to closed and new states to frontier
    while frontier:
        node = frontier.pop()

        if not node.state in closedStates:
            closedStates.append(node.state)
            if problem.isGoalState(node.state):
                return node.path

            # If nodes state is not a goal state, continue expanding the frontier with its successors
            succs = problem.getSuccessors(node.state)
            for successor in succs:
                s, d, c = successor
                newPath = node.path + [d]
                frontier.push(Node(s, newPath, c))

    # no solution could be found
    return []

def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"
    closedStates = []
    startState = problem.getStartState()
    frontier = util.Queue()
    frontier.push(Node(startState, [], 0))

    # until solution is found, go through frontier, adding states to closed and new states to frontier
    while frontier:
        node = frontier.pop()

        if not node.state in closedStates:
            closedStates.append(node.state)
            if problem.isGoalState(node.state):
                return node.path

            # If nodes state is not a goal state, continue expanding the frontier with its successors
            succs = problem.getSuccessors(node.state)
            for successor in succs:
                s, d, c = successor
                newPath = node.path + [d]
                frontier.push(Node(s, newPath, c))

    # no solution could be found
    return []

def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"
    closedStates = []
    startState = problem.getStartState()
    frontier = util.PriorityQueue()
    frontier.push(Node(startState, [], 0), 0)

    # until solution is found, go through frontier, adding states to closed and new states to frontier
    while frontier:
        node = frontier.pop()

        if not node.state in closedStates:
            closedStates.append(node.state)
            if problem.isGoalState(node.state):
                return node.path

            # If nodes state is not a goal state, continue expanding the frontier with its successors
            succs = problem.getSuccessors(node.state)
            for successor in succs:
                s, d, c = successor
                newPath = node.path + [d]
                newCost = node.cost + c
                frontier.push(Node(s, newPath, newCost), newCost)

    # no solution could be found
    return []

def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"
    closedStates = []
    startState = problem.getStartState()
    frontier = util.PriorityQueue()
    frontier.push(Node(startState, [], 0), 0)

    # until solution is found, go through frontier, adding states to closed and new states to frontier
    while frontier:
        node = frontier.pop()

        if not node.state in closedStates:
            closedStates.append(node.state)
            if problem.isGoalState(node.state):
                return node.path

            # If nodes state is not a goal state, continue expanding the frontier with its successors
            succs = problem.getSuccessors(node.state)
            for successor in succs:
                s, d, c = successor
                newPath = node.path + [d]
                newCost = node.cost + c
                frontier.push(Node(s, newPath, newCost), newCost + heuristic(s, problem))

    # no solution could be found
    return []

# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch
