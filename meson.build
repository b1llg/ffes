#--- Project
project('GridPack', 'fortran', meson_version: '>=0.49')

#--- Dependency
netcdf_dep = dependency('netcdf',language: 'fortran',required: true)
#petsc_dep = dependency('petsc', required:false)


executable('gridpack', 
    files(
        'main.f90',
        'm_mesh.f90', 
        'm_precision.f90', 
        'm_io.f90'),
        dependencies : [netcdf_dep]#,petsc_dep]
)
