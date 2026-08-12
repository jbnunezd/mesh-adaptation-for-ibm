#----------------------------------------------------------------------------------------------------------------------#
# PREPROCESSOR EXECUTABLE
#----------------------------------------------------------------------------------------------------------------------#
SET(EXECUTABLE_NAME "mesh-adaptation-for-ibm")
ADD_EXECUTABLE(${EXECUTABLE_NAME} "./src/mesh-adaptation-for-ibm.f90")
#----------------------------------------------------------------------------------------------------------------------#
# TARGET LINKED LIBRARIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_GLOBALS})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_CONFIGFILES_TOOLS})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_DATA_STRUCTURES})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_HDF5_TOOLS})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_NUMERICS_TOOLS})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_MESH_ADAPTATION})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${LINKEDLIBS_HDF5})
TARGET_LINK_LIBRARIES(${EXECUTABLE_NAME} PUBLIC ${INTERNALLIBS})
#----------------------------------------------------------------------------------------------------------------------#
# TARGET INCLUDE DIRECTORIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_GLOBALS})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_CONFIGFILES_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_DATA_STRUCTURES})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_HDF5_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_NUMERICS_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_MESH_ADAPTATION})
TARGET_INCLUDE_DIRECTORIES(${EXECUTABLE_NAME} PUBLIC ${INCLUDE_DIRECTORIES_HDF5})
#----------------------------------------------------------------------------------------------------------------------#
# TARGET PROPERTIES
#----------------------------------------------------------------------------------------------------------------------#
SET_TARGET_PROPERTIES(${EXECUTABLE_NAME} PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
#----------------------------------------------------------------------------------------------------------------------#
# TARGET INSTALL
#----------------------------------------------------------------------------------------------------------------------#
INSTALL(TARGETS ${EXECUTABLE_NAME} DESTINATION "bin")
#----------------------------------------------------------------------------------------------------------------------#
