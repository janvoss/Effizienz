from sympy import Eq, symbols, solve, diff, lambdify, Rational, latex, log
from lets_plot import *
import numpy as np  # Import NumPy

# Setup Lets-Plot for HTML output
LetsPlot.setup_html()

# Define symbols
X, alpha, i, g, x1, x2, U, lambda_ = symbols('X alpha i g x1 x2 U lambda_')

# Given values for substitution
given_values = {alpha: .5, i: .1, g: .05, X: 100}

# Define utility function
Utility = Eq(U, log(x1) + 1 / (1 + i) * log(x2)).subs(given_values)

# Define budget constraint
budget = Eq(x2, (X - x1) * (1 + g)).subs(given_values)

# Lagrange function
L = Utility.rhs + lambda_ * (budget.lhs - budget.rhs)

# Solve for optimal values
solutions = solve((diff(L, x1), diff(L, x2), diff(L, lambda_)), (x1, x2, lambda_), dict=True)

# Substitute solution into utility function
U_opt = Utility.rhs.subs(solutions[0])

# Extract optimal values of x1 and x2
x1_opt = float(solutions[0][x1])
x2_opt = float(solutions[0][x2])

# Budget line endpoints
x1_budget = float(given_values[X])  # Maximum value of x1
x2_budget = float(given_values[X] * (1 + given_values[g]))  # Maximum value of x2

# Indifference curve utility levels
U_values = [float(U_opt) - 1, float(U_opt), float(U_opt) + 1]
colors = ['blue', 'green', 'blue']

# Budget line function
budget_line = lambdify(x1, solve(budget, x2)[0].subs(given_values))

# Generate x-values for plotting
x_values = np.arange(0.01, x1_budget + 0.5, 0.1)
data = {'x': x_values.tolist()}

# Plotting with Lets-Plot
p = ggplot() + \
    geom_function(aes('x'), data=data,
                  fun=lambda t: budget_line(t), color='red')

for idx, u in enumerate(U_values):
    Indifference_curve = solve(Utility.subs(U, u), x2)[0]
    Ind_graph = lambdify(x1, Indifference_curve)
    p += geom_function(aes('x'), data=data,
                       fun=lambda t: Ind_graph(t), color=colors[idx], size=1) + \
         geom_text(x=x1_budget + 0.5,
                   y=Ind_graph(x1_budget),
                   label=f"U={round(u, 2)}")

p += geom_segment(x=x1_opt, y=0,
                  xend=x1_opt, yend=x2_opt,
                  linetype='dashed', size=0.5) + \
     geom_segment(x=0, y=x2_opt,
                  xend=x1_opt, yend=x2_opt,
                  linetype='dashed', size=0.5) + \
     geom_point(x=x1_opt, y=x2_opt) + \
     labs(title='Intertemporale Konsumoptimierung im Zwei-Perioden-Model',
          x=r'\(x_0\)',
          y=r'\(x_1\)',
          caption='Abbildung: Jan S. Voßwinkel') + \
     coord_cartesian(xlim=[0, x1_budget + 5.
], ylim=[0, x2_budget + 10]) + \
     scale_x_continuous(breaks=[0, round(x1_opt, 2), round(x1_budget)]) + \
     scale_y_continuous(breaks=[0, round(x2_opt, 2), round(x2_budget)]) + \
     theme_light()

# Display the plot
p.show()
