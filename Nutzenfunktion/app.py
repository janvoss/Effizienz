import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import plotly.graph_objects as go

from shiny import App, ui, render
from sympy import symbols, Eq, solve, lambdify, latex
from shinywidgets import output_widget, render_widget  # Für Plotly Integration

# ----------------------------------------------------
# 1. Symbole definieren (Globale Variablen)
# ----------------------------------------------------
x1, x2, U, a_sym = symbols('x1 x2 U a')
equation_sym = Eq(U, x1**0.5 * x2**0.5 - a_sym * x1**2)
U_expr = equation_sym.rhs

# ----------------------------------------------------
# 2. UI Definition (app_ui)
# ----------------------------------------------------
app_ui = ui.page_fluid(
    ui.tags.title("Nutzenfunktionen & Indifferenzkurven"),
    ui.layout_sidebar(
        ui.sidebar(
            ui.h4("Kontrollen"),
            ui.input_numeric("a_input", "Faktor a festlegen", value=0.5, min=0.01, step=0.01),
            ui.input_numeric("U_level_1", "U-Level 1 (Indifferenzkurve)", value=1.0),
            ui.input_numeric("U_level_2", "U-Level 2 (Indifferenzkurve)", value=2.0),
            ui.input_numeric("U_level_3", "U-Level 3 (Indifferenzkurve)", value=3.0),
        ),
        ui.h2("Ökonomische Modellierung mit SymPy, Matplotlib & Plotly"),
        ui.layout_columns(
            ui.card(
                ui.card_header("Formeln"),
                ui.markdown("#### 1. 📝 Allgemeine Nutzenfunktion"),
                ui.output_ui("general_equation_latex"),
                ui.hr(),
                ui.markdown("#### 2. 🔢 Gelöste Indifferenzkurve ($x_2(x_1)$)"),
                ui.output_ui("specific_solution_latex"),
            ),
            ui.card(
                ui.card_header("Debugging"),
                ui.markdown("#### 🔍 Generierter SymPy Code (Debug)"),
                ui.output_code("sympy_operations_debug"),
            ),
            col_widths=(6, 6)
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("2D-Plot: Nutzenfunktion $U(x_1)$ (mit $x_2=1$)"),
                ui.output_plot("plot_utility_2d"),
            ),
            ui.card(
                ui.card_header("2D-Plot: Indifferenzkurven $x_2(x_1)$"),
                ui.output_plot("plot_indifference_curves"),
            ),
            col_widths=(6, 6)
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("3D-Plot: Nutzenfunktion $U(x_1, x_2)$ (Plotly, interaktiv)"),
                output_widget("plot_utility_3d"),  # Widget statt output_plot
            ),
            col_widths=(12,)
        ),
    ),
)

# ----------------------------------------------------
# 3. Server-Logik Definition (server)
# ----------------------------------------------------
def server(input, output, session):

    @render.ui
    def general_equation_latex():
        latex_string = latex(equation_sym)
        return ui.markdown(f"$$\\text{{Nutzenfunktion:}} \\quad {latex_string}$$")

    @render.ui
    def specific_solution_latex():
        a_value = input.a_input()
        equation = equation_sym.subs(a_sym, a_value)
        try:
            solution_x2 = solve(equation, x2)[0]
        except (IndexError, TypeError):
            return ui.markdown("**Fehler:** Keine analytische Lösung für $x_2$ gefunden.")
        solution_eq = Eq(x2, solution_x2)
        latex_string = latex(solution_eq)
        return ui.markdown(f"""
        **Faktor $a = {a_value}$**
        $$\\text{{Lösung für}} \\; x_2: \\quad {latex_string}$$
        """)

    @render.code
    def sympy_operations_debug():
        a_value = input.a_input()
        equation = equation_sym.subs(a_sym, a_value)
        try:
            solution_x2 = solve(equation, x2)[0]
            x2_func = lambdify((x1, U), solution_x2)
        except Exception as e:
            return f"# Fehler: {e}"
        return f"""
# Aktueller Faktor a: {a_value}
# Lösung für x2:
x2 = {solution_x2}
# Lambdified Funktion: {x2_func}
"""

    @render.plot(alt="2D Plot der Nutzenfunktion U(x1) mit x2=1")
    def plot_utility_2d():
        a_value = input.a_input()
        U_expr_2d = U_expr.subs(x2, 1)
        U_func_2d = lambdify((x1, a_sym), U_expr_2d)
        x1_range = np.linspace(0.1, 5, 100)
        U_val = U_func_2d(x1_range, a_value)
        fig, ax = plt.subplots()
        ax.plot(x1_range, U_val, label=f'a = {a_value}')
        ax.set_title(f'Nutzenfunktion U($x_1$) | $x_2=1$, $a$={a_value}')
        ax.set_xlabel('Gut $x_1$')
        ax.set_ylabel('Nutzen $U$')
        ax.grid(True, linestyle='--')
        ax.legend()
        fig.tight_layout()
        return fig

    @render.plot(alt="2D Plot der Indifferenzkurven")
    def plot_indifference_curves():
        a_value = input.a_input()
        equation = equation_sym.subs(a_sym, a_value)
        try:
            solution_x2 = solve(equation, x2)[0]
            x2_func = lambdify((x1, U), solution_x2)
        except Exception:
            fig, ax = plt.subplots()
            ax.text(0.5, 0.5, "Keine Indifferenzkurven darstellbar",
                    ha='center', va='center', fontsize=12)
            ax.set_title("Fehler bei der Lösung")
            ax.axis('off')
            return fig
        U_levels = [input.U_level_1(), input.U_level_2(), input.U_level_3()]
        x1_range = np.linspace(0.1, 5, 100)
        fig, ax = plt.subplots()
        for U_val in U_levels:
            x2_val = x2_func(x1_range, U_val)
            mask = (np.imag(x2_val) == 0) & (np.real(x2_val) > 0)
            ax.plot(x1_range[mask], np.real(x2_val[mask]),
                    label=f'U = {U_val}', linewidth=2)
        ax.set_title(f'Indifferenzkurven (a = {a_value})')
        ax.set_xlabel('Gut $x_1$')
        ax.set_ylabel('Gut $x_2$')
        ax.set_ylim(bottom=0)
        ax.grid(True, linestyle=':')
        ax.legend(title='Nutzen-Level')
        fig.tight_layout()
        return fig

    @render_widget
    def plot_utility_3d():
        a_value = input.a_input()
        U_func = lambdify((x1, x2, a_sym), U_expr)
        x1_range = np.linspace(0.1, 5, 50)
        x2_range = np.linspace(0.1, 5, 50)
        X1, X2 = np.meshgrid(x1_range, x2_range)
        U_val = U_func(X1, X2, a_value)
        fig = go.Figure(data=[go.Surface(z=U_val, x=X1, y=X2,
                                         colorscale='Viridis',
                                         opacity=0.9)])
        fig.update_layout(
            title=f'Nutzenfunktion U(x₁, x₂ | a={a_value})',
            scene=dict(
                xaxis_title='Gut x₁',
                yaxis_title='Gut x₂',
                zaxis_title='Nutzen U',
                aspectratio=dict(x=1, y=1, z=0.7)
            ),
            margin=dict(l=0, r=0, b=0, t=30),
            height=600
        )
        return fig

# ----------------------------------------------------
# 4. App-Initialisierung
# ----------------------------------------------------
app = App(app_ui, server)
