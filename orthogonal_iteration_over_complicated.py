#!/bin/python3

import numpy as np
import matplotlib.pyplot as plt

np.random.seed()

# ==================================================
# DEFINE MATRICES AND SELECT ONE AT RANDOM
# ==================================================

matrices = {

    'Tridiagonal positive':
    np.array([
        [ 2, -1,  0],
        [-1,  2, -1],
        [ 0, -1,  2]
    ]),

    'All positive':
    np.array([
        [ 2,  1,  1],
        [ 1,  2,  1],
        [ 1,  1,  2]
    ]),

    'Tridiagonal positive off-diagonal':
    np.array([
        [ 2,  1,  0],
        [ 1,  2,  1],
        [ 0,  1,  2]
    ]),

    'Tridiagonal ones':
    np.array([
        [ 1,  1,  0],
        [ 1,  1,  1],
        [ 0,  1,  1]
    ]),

    'Zero diagonal':
    np.array([
        [ 0,  1,  0],
        [ 1,  0,  1],
        [ 0,  1,  0]
    ]),
    
    'Asymmetric':
    np.array([
        [ 1,  1,  1],
        [ 1,  1,  1],
        [ 0,  1,  1]
    ]),

    'Random symmetric':
    (
        lambda B: (B + B.T) / 2
    )(np.random.randn(3, 3)),

    'Destroys a subspace':
    np.array([
        [ 1,  -1,  0],
        [ -1,  1,  0],
        [ 0,  0,  1]
    ]),


    'Constructed':
    # np.array([[281, 120,   0],
    #    [120, 164,   0],
    #    [  0,   0,   1]]),

    np.array([
        [ 8,  5,  0],
        [ 5,  -8,  0],
        [ 0,  0,  1]])@ np.array([
        [ 4,  0,  0],
        [ 0,  1,  0],
        [ 0,  0,  1]
    ])@ (np.array([
        [ 8,  5,  0],
        [ 5,  -8,  0],
        [ 0,  0,  1]]).T),
    
    'Fibonacci':
    np.array([
        [ 1,  1,  0],
        [ 1,  0,  0],
        [ 0,  0,  1]
    ]),
    
   'Non-Normal':
    np.array([
        [ 1,  10,  0],
        [ 0,  2,  0],
        [ 0,  0,  1]
    ]),
}


# Randomly select a matrix
matrix_name = np.random.choice(list(matrices.keys()))
# or to just pick the final matrix
# matrix_name = list(matrices.keys())[-1]

A = matrices[matrix_name]

print("\nSelected matrix:", matrix_name)


# ==================================================
# CALCULATE THEORETICAL EIGENVALUES/VECTORS
# ==================================================

eigenvalues, eigenvectors = np.linalg.eig(A)

print("\nTheoretical eigenvalues:")
print(eigenvalues)

print("\nTheoretical eigenvectors:")
print(eigenvectors)


# ==================================================
# MATRIX PROPERTY DIAGNOSTICS
# ==================================================

def matrix_diagnostics(A, eigenvalues, eigenvectors):

    n = A.shape[0]
    I = np.eye(n)

    eps = np.finfo(float).eps

    # Basic properties
    symmetric_residual = (
        np.linalg.norm(A - A.T, 'fro')
        / max(np.linalg.norm(A, 'fro'), eps)
    )

    is_symmetric = np.allclose(A, A.T)

    # Normality: A.T @ A = A @ A.T
    normality_residual = (
        np.linalg.norm(A.T @ A - A @ A.T, 'fro')
        / max(np.linalg.norm(A, 'fro')**2, eps)
    )

    is_normal = np.allclose(
        A.T @ A,
        A @ A.T
    )

    # Orthogonality: A.T @ A = I
    orthogonality_residual = np.linalg.norm(
        A.T @ A - I, 'fro'
    ) / np.sqrt(n)

    is_orthogonal = np.allclose(
        A.T @ A, I
    )

    # Rank and singular values
    singular_values = np.linalg.svd(
        A, compute_uv=False
    )

    rank = np.linalg.matrix_rank(A)

    condition_number = np.linalg.cond(A, 2)

    # Determinant and trace
    determinant = np.linalg.det(A)
    trace = np.trace(A)

    # Spectral radius
    spectral_radius = np.max(np.abs(eigenvalues))

    # Eigenvalues sorted by decreasing magnitude
    sorted_indices = np.argsort(
        np.abs(eigenvalues)
    )[::-1]

    sorted_eigenvalues = eigenvalues[sorted_indices]

    eigenvalue_magnitudes = np.abs(sorted_eigenvalues)

    # Eigenvalue separation ratios
    ratios = []

    for i in range(n - 1):

        if eigenvalue_magnitudes[i] > eps:
            ratio = (
                eigenvalue_magnitudes[i + 1]
                / eigenvalue_magnitudes[i]
            )
        else:
            ratio = np.nan

        ratios.append(ratio)

    # Positive definiteness (only meaningful for symmetric matrices)
    if is_symmetric:

        symmetric_eigenvalues = np.linalg.eigvalsh(A)

        positive_definite = np.all(
            symmetric_eigenvalues > eps
        )

        positive_semidefinite = np.all(
            symmetric_eigenvalues >= -eps
        )

        negative_definite = np.all(
            symmetric_eigenvalues < -eps
        )

    else:

        positive_definite = None
        positive_semidefinite = None
        negative_definite = None

    # Conditioning of the eigenvector matrix
    eigenvector_condition = np.linalg.cond(
        eigenvectors
    )

    # Relative distance from singularity
    relative_smallest_singular_value = (
        singular_values[-1] / singular_values[0]
        if singular_values[0] > eps
        else 0.0
    )

    return {

        'Symmetric': is_symmetric,
        'Symmetry residual': symmetric_residual,

        'Normal': is_normal,
        'Normality residual': normality_residual,

        'Orthogonal matrix': is_orthogonal,
        'Orthogonality residual': orthogonality_residual,

        'Positive definite': positive_definite,
        'Positive semidefinite': positive_semidefinite,
        'Negative definite': negative_definite,

        'Rank': rank,
        'Condition number': condition_number,

        'Determinant': determinant,
        'Trace': trace,

        'Spectral radius': spectral_radius,

        'Eigenvalues (magnitude order)': sorted_eigenvalues,

        'Eigenvalue ratios': ratios,

        'Eigenvector condition number': eigenvector_condition,

        'Relative smallest singular value':
            relative_smallest_singular_value
    }




# ==================================================
# DEFINE STARTING VECTORS
# ==================================================

initial_vectors = {
    'Vector 1': np.array([1., 1., 1.]),
    'Vector 2': np.array([1., -1., 0.]),
    'Vector 3': np.array([1., 1., -2.])
}

# Number of original vectors to retain
n_keep = 0

# Random seed for reproducibility
# np.random.seed(42)

# Keep the first n_keep vectors
initial_vectors = dict(
    list(initial_vectors.items())[:n_keep]
)

# Generate random replacements
n_random_vectors = 3 - n_keep

for i in range(n_random_vectors):
    initial_vectors[f'Random {i+1}'] = np.random.randn(3)


print("Matrix A:")
print(A)

print("\nInitial vectors:")

starting_vectors = {
    name: v.copy()
    for name, v in initial_vectors.items()
}

for name, v in starting_vectors.items():
    print(f"{name}: {v}")


# ==================================================
# ORTHOGONALISATION FUNCTION
# ==================================================

def orthogonalise(V):
    """Orthogonalise columns of V without normalising them."""

    Q = np.zeros_like(V, dtype=float)

    for i in range(V.shape[1]):

        v = V[:, i].copy()

        for j in range(i):
            v -= (
                np.dot(Q[:, j], v)
                / np.dot(Q[:, j], Q[:, j])
            ) * Q[:, j]

        Q[:, i] = v

    return Q


# ==================================================
# ORTHOGONAL ITERATION
# ==================================================

n_iterations = 10

names = list(initial_vectors.keys())

histories = {
    name: [v.copy()]
    for name, v in initial_vectors.items()
}

for iteration in range(n_iterations):

    # Multiply each vector by A
    V = np.column_stack([
        A @ initial_vectors[name]
        for name in names
    ])

    # Orthogonalise without normalising
    Q = orthogonalise(V)

    # Update vectors and store history
    for i, name in enumerate(names):

        initial_vectors[name] = Q[:, i].copy()

        histories[name].append(
            Q[:, i].copy()
        )


# Convert histories to arrays
for name in names:
    histories[name] = np.array(histories[name])


print("\nFinal vectors:")

for name, v in initial_vectors.items():
    print(f"{name}: {v}")


# ==================================================
# CREATE FIGURE LAYOUT
# ==================================================

fig = plt.figure(figsize=(18, 15))

gs = fig.add_gridspec(
    3, 6,
    height_ratios=[0.1, 4, 1],
    width_ratios=[1, 1, 1, 1, 1, 1],
    hspace=0.45,
    wspace=0.4
)


# ==================================================
# THREE NORMALISED VECTOR COMPONENTS (TOP ROW)
# ==================================================

axes = [
    fig.add_subplot(gs[0, 0:2]),
    fig.add_subplot(gs[0, 2:4]),
    fig.add_subplot(gs[0, 4:6])
]


# Fade settings (relative to initial magnitude)

fade_start = 1e-2
magnitude_cutoff = 1e-14


def vector_opacity(magnitude, initial_magnitude):
    """Calculate opacity from relative vector magnitude."""

    relative_magnitude = magnitude / initial_magnitude

    if relative_magnitude <= magnitude_cutoff:
        return 0.0

    if relative_magnitude >= fade_start:
        return 1.0

    # Logarithmic fade
    alpha = (
        np.log10(relative_magnitude)
        - np.log10(magnitude_cutoff)
    ) / (
        np.log10(fade_start)
        - np.log10(magnitude_cutoff)
    )

    return alpha


# Plot normalised components with magnitude-dependent fading

for i, (name, history) in enumerate(histories.items()):

    color = f'C{i}'

    magnitudes = np.linalg.norm(
        history,
        axis=1
    )

    initial_magnitude = magnitudes[0]

    # Calculate normalised components only where reliable
    normalised = np.full_like(history, np.nan)

    for iteration in range(len(history)):

        if magnitudes[iteration] > (
            magnitude_cutoff * initial_magnitude
        ):

            normalised[iteration] = (
                history[iteration] / magnitudes[iteration]
            )

    # Add legend entry
    for j in range(3):
        axes[j].plot(
            [],
            [],
            'o-',
            color=color,
            label=name
        )

    # Plot individual components
    for j in range(3):

        for iteration in range(len(history)):

            alpha = vector_opacity(
                magnitudes[iteration],
                initial_magnitude
            )

            if alpha == 0:
                continue

            # Plot component marker
            axes[j].plot(
                iteration,
                normalised[iteration, j],
                'o',
                color=color,
                alpha=alpha
            )

            # Connect to previous point if reliable
            if iteration > 0:

                previous_alpha = vector_opacity(
                    magnitudes[iteration - 1],
                    initial_magnitude
                )

                if previous_alpha > 0:

                    axes[j].plot(
                        [iteration - 1, iteration],
                        [
                            normalised[iteration - 1, j],
                            normalised[iteration, j]
                        ],
                        color=color,
                        alpha=min(alpha, previous_alpha),
                        linewidth=1.5
                    )


# Add theoretical eigenvector reference lines

for j, ax in enumerate(axes):

    for k in range(3):

        limit = eigenvectors[j, k]

        ax.axhline(
            limit,
            color=f'C{k+1}',
            linestyle='--',
            alpha=0.8,
            label=f'Eigenvector {k+1}'
        )

        ax.axhline(
            -limit,
            color=f'C{k+1}',
            linestyle='--',
            alpha=0.8
        )

    ax.set_title(f'Component {j+1}')

    ax.set_xlabel('Iteration')
    ax.set_ylabel('Normalised value')

    ax.set_ylim(-1, 1)

    ax.grid(True)
    ax.legend(fontsize=7)


# ==================================================
# MATRIX HEATMAP AND PROPERTY DIAGNOSTICS
# ==================================================

diagnostics = matrix_diagnostics(
    A, eigenvalues, eigenvectors
)

# Divide existing matrix plotting area into two panels
matrix_gs = gs[1, 0:3].subgridspec(
    1, 2,
    width_ratios=[1, 1.35],
    wspace=0.3
)



ax_matrix = fig.add_subplot(matrix_gs[0, 0])
ax_diag = fig.add_subplot(matrix_gs[0, 1])


# --------------------------------------------------
# MATRIX HEATMAP
# --------------------------------------------------

im = ax_matrix.imshow(
    A,
    cmap='viridis'
)

fig.colorbar(
    im,
    ax=ax_matrix,
    label='Matrix value',
    fraction=0.046,
    pad=0.04
)

for i in range(A.shape[0]):

    for j in range(A.shape[1]):

        ax_matrix.text(
            j, i,
            f'{A[i, j]:.2f}',
            ha='center',
            va='center',
            color='white',
            fontsize=10
        )

ax_matrix.set_title('Matrix A')

# --------------------------------------------------
# PROPERTY DISPLAY
# --------------------------------------------------

ax_diag.axis('off')

def status(value):

    if value is None:
        return 'N/A'

    return 'YES' if value else 'NO'


def fmt(value):

    if value is None:
        return 'N/A'

    if np.isinf(value):
        return '∞'

    if np.isnan(value):
        return 'undefined'

    return f'{value:.3e}'


d = diagnostics

lines = [

    'MATRIX PROPERTIES',
    '',
    f"Symmetric:          {status(d['Symmetric'])}",
    f"Normal:             {status(d['Normal'])}",
    f"Orthogonal:         {status(d['Orthogonal matrix'])}",
    '',
    f"Symmetry residual:  {fmt(d['Symmetry residual'])}",
    f"Normality residual: {fmt(d['Normality residual'])}",
    f"Orthogonality:      {fmt(d['Orthogonality residual'])}",
    '',
    f"Rank:               {d['Rank']} / {A.shape[0]}",
    f"Condition number:   {fmt(d['Condition number'])}",
    f"Determinant:        {fmt(d['Determinant'])}",
    f"Trace:              {fmt(d['Trace'])}",
    '',
    f"Spectral radius:    {fmt(d['Spectral radius'])}",
    f"Eigenvector cond.:  {fmt(d['Eigenvector condition number'])}",
    f"Smallest SV ratio:  {fmt(d['Relative smallest singular value'])}",
    '',
    'DEFINITENESS',
    f"Positive definite:  {status(d['Positive definite'])}",
    f"Positive semidef.:  {status(d['Positive semidefinite'])}",
    f"Negative definite:  {status(d['Negative definite'])}",
    '',
    'EIGENVALUE MAGNITUDES',
]

for i, value in enumerate(
    d['Eigenvalues (magnitude order)']
):

    lines.append(
        f'|λ{i+1}| = {abs(value):.5g}'
    )

lines.append('')
lines.append('CONVERGENCE RATIOS')

for i, ratio in enumerate(d['Eigenvalue ratios']):

    lines.append(
        f'|λ{i+2}| / |λ{i+1}| = {fmt(ratio)}'
    )

ax_diag.text(
    0.0, 1.0,
    '\n'.join(lines),
    transform=ax_diag.transAxes,
    verticalalignment='top',
    horizontalalignment='left',
    fontsize=7.5,
    family='monospace'
)

ax_diag.set_title('Numerical diagnostics', fontsize=10)



# ==================================================
# EIGENVECTOR ALIGNMENT DIAGNOSTICS
# ==================================================

def vector_eigen_diagnostics(A, v):

    v = np.asarray(v, dtype=float)

    norm_v = np.linalg.norm(v)

    if norm_v < 1e-14:
        return None

    Av = A @ v

    norm_Av = np.linalg.norm(Av)

    if norm_Av < 1e-14:
        return {
            'Angle': np.nan,
            'Alignment angle': np.nan,
            'Rayleigh quotient': 0.0,
            'Residual': 0.0 if norm_v > 0 else np.nan
        }

    # Angle between v and Av
    cosine = np.dot(v, Av) / (norm_v * norm_Av)

    cosine = np.clip(cosine, -1.0, 1.0)

    angle = np.degrees(np.arccos(cosine))

    # Acute alignment angle
    alignment_angle = np.degrees(
        np.arccos(abs(cosine))
    )

    # Best-fit eigenvalue (Rayleigh quotient)
    eigenvalue = np.dot(v, Av) / np.dot(v, v)

    # Eigenvector residual
    residual = np.linalg.norm(
        Av - eigenvalue * v
    ) / (
        norm_Av + abs(eigenvalue) * norm_v
    )

    return {
        'Angle': angle,
        'Alignment angle': alignment_angle,
        'Rayleigh quotient': eigenvalue,
        'Residual': residual
    }


# ==================================================
# CHECK THEORETICAL EIGENVECTORS
# ==================================================

print("\nTHEORETICAL EIGENVECTOR CHECKS")

for k in range(len(eigenvalues)):

    v = eigenvectors[:, k]

    result = vector_eigen_diagnostics(A, v)

    if result is None:
        continue

    print(
        f"\nEigenvector {k+1}:"
    )

    print(
        f"  Theoretical eigenvalue: {eigenvalues[k]:.8f}"
    )

    print(
        f"  Calculated eigenvalue:  "
        f"{result['Rayleigh quotient']:.8f}"
    )

    print(
        f"  Angle v to Av:          "
        f"{result['Angle']:.6f} degrees"
    )

    print(
        f"  Alignment angle:        "
        f"{result['Alignment angle']:.6f} degrees"
    )

    print(
        f"  Eigenvector residual:   "
        f"{result['Residual']:.3e}"
    )


# ==================================================
# CHECK FINAL ITERATED VECTORS
# ==================================================

print("\nFINAL ITERATED VECTOR CHECKS")

for name, v in initial_vectors.items():

    result = vector_eigen_diagnostics(A, v)

    print(f"\n{name}:")

    if result is None:
        print("  Zero vector: no direction to test")
        continue

    print(
        f"  Rayleigh quotient: {result['Rayleigh quotient']:.8f}"
    )

    print(
        f"  Angle v to Av: "
        f"{result['Angle']:.6f} degrees"
    )

    print(
        f"  Alignment angle: "
        f"{result['Alignment angle']:.6f} degrees"
    )

    print(
        f"  Eigenvector residual: "
        f"{result['Residual']:.3e}"
    )


# ==================================================
# EIGENVECTOR CONVERGENCE HISTORY
# ==================================================

ax_angle = fig.add_subplot(gs[2, 0:2])
ax_residual = fig.add_subplot(gs[2, 2])

for name, history in histories.items():

    angles = []
    residuals = []

    for v in history:

        result = vector_eigen_diagnostics(A, v)

        if result is None:
            angles.append(np.nan)
            residuals.append(np.nan)

        else:
            angles.append(result['Alignment angle'])
            residuals.append(result['Residual'])

    ax_angle.plot(
        angles,
        'o-',
        label=name
    )

    ax_residual.plot(
        residuals,
        'o-',
        label=name
    )

ax_angle.set_title('Eigenvector alignment')
ax_angle.set_xlabel('Iteration')
ax_angle.set_ylabel('Alignment angle (degrees)')
ax_angle.grid(True)
ax_angle.legend(fontsize=8)

ax_residual.set_title('Eigenvector residual')
ax_residual.set_xlabel('Iteration')
ax_residual.set_ylabel('Relative residual')
ax_residual.set_yscale('log')
ax_residual.grid(True)
ax_residual.legend(fontsize=7)




# ==================================================
# GROWTH OF VECTOR MAGNITUDES (BOTTOM MIDDLE)
# ==================================================

ax_mag = fig.add_subplot(gs[2, 3:6])

for name, history in histories.items():

    magnitudes = np.linalg.norm(
        history,
        axis=1
    )

    ax_mag.plot(
        magnitudes,
        'o-',
        label=name
    )


# Add theoretical exponential growth lines

iterations = np.arange(n_iterations + 1)

for k, eigenvalue in enumerate(eigenvalues):

    # Theoretical growth from unit initial magnitude
    theoretical_growth = np.abs(eigenvalue ** iterations)

    ax_mag.plot(
        iterations,
        theoretical_growth,
        color=f'C{k+1}',
        linestyle='--',
        alpha=0.8,
        label=f'Eigenvalue {k+1} = {eigenvalue:.3f}'
    )


ax_mag.set_xlabel('Iteration')
ax_mag.set_ylabel('Vector magnitude')

ax_mag.set_title('Growth of Trial Vectors')

ax_mag.set_yscale('log')

ax_mag.legend(fontsize=8)
ax_mag.grid(True)


# ==================================================
# 3D PLOT: THEORETICAL EIGENVECTORS AND ITERATED VECTORS
# ==================================================


ax_eig = fig.add_subplot(gs[1, 3:6], projection="3d")
# Plot theoretical eigenvector directions as black arrows.
# Eigenvectors are normalised here so they can be compared with
# the normalised directions of the iterated trial vectors.
plotted_eigenvectors = 0

for k, eigenvalue in enumerate(eigenvalues):
    eigenvector = eigenvectors[:, k]

    # Ordinary real 3D axes cannot display genuinely complex directions.
    if not np.allclose(eigenvector.imag, 0, atol=1e-10):
        print(
            f"Skipping eigenvector {k + 1}: it is genuinely complex "
            "and cannot be represented as a real 3D direction."
        )
        continue

    direction = np.real(eigenvector)
    norm = np.linalg.norm(direction)
    if norm == 0:
        continue
    direction = direction / norm

    ax_eig.quiver(
        0, 0, 0,
        direction[0], direction[1], direction[2],
        color="black",
        linewidth=2,
        arrow_length_ratio=0.12,
        label=f"Eigenvector {k + 1}, λ = {eigenvalue.real:.3f}"
    )
    ax_eig.quiver(
        0, 0, 0,
        -direction[0], -direction[1], -direction[2],
        color="black",
        linewidth=1,
        alpha=0.25,
        arrow_length_ratio=0.12
    )
    plotted_eigenvectors += 1


# Plot each trial vector's direction at every iteration.
# Normalising for display keeps the trajectory visible even when the
# actual vector magnitudes grow or shrink dramatically.
for i, (name, history) in enumerate(histories.items()):
    color = f"C{i}"

    magnitudes = np.linalg.norm(history, axis=1)
    valid = magnitudes > 1e-14

    directions = np.full_like(history, np.nan, dtype=float)
    directions[valid] = history[valid] / magnitudes[valid, np.newaxis]

    valid_directions = directions[valid]

    if len(valid_directions) == 0:
        continue

    # Connect successive directions to show the path through iterations.
    ax_eig.plot(
        valid_directions[:, 0],
        valid_directions[:, 1],
        valid_directions[:, 2],
        color=color,
        marker="o",
        markersize=3.5,
        linewidth=1.5,
        label=f"{name} (iterations)"
    )

    # Highlight the starting and final directions.
    first = valid_directions[0]
    last = valid_directions[-1]

    ax_eig.scatter(
        first[0], first[1], first[2],
        color=color,
        marker="s",
        s=35,
        edgecolors="black",
        linewidths=0.5
    )
    ax_eig.scatter(
        last[0], last[1], last[2],
        color=color,
        marker="*",
        s=85,
        edgecolors="black",
        linewidths=0.5
    )

# Unit-sphere limits make the direction comparison geometrically fair.
ax_eig.set_xlim(-1, 1)
ax_eig.set_ylim(-1, 1)
ax_eig.set_zlim(-1, 1)
ax_eig.set_box_aspect((1, 1, 1))
ax_eig.set_xlabel("x")
ax_eig.set_ylabel("y")
ax_eig.set_zlabel("z")
ax_eig.set_title("Eigenvectors and iterated vector directions")
ax_eig.view_init(elev=22, azim=35)

if plotted_eigenvectors:
    ax_eig.legend(fontsize=6, loc="upper left")
else:
    ax_eig.text2D(
        0.1, 0.5,
        "No real eigenvectors to display.",
        transform=ax_eig.transAxes
    )


# ==================================================
# FINAL LAYOUT
# ==================================================

eigenvalue_text = ', '.join(
    f'{value:.3f}' for value in eigenvalues
)

fig.suptitle(
    f'Orthogonal Iteration and Vector Evolution: '
    f'{matrix_name}\n'
    f'(Eigenvalues: {eigenvalue_text})',
    fontsize=15
)

fig.tight_layout(rect=[0, 0, 1, 0.96])

plt.show()
