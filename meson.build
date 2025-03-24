project('ffes', 'fortran', meson_version: '>=0.49')

exodus_dep = dependency('exodus', required = true)

executable('ffes', 
    files(
        'ffes.f90',
        'm_mesh.f90', 
        'm_precision.f90', 
        'm_io.f90'))