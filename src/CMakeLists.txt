###| CMAKE qtermwidget/src |###

#| Set required version of cmake
cmake_minimum_required ( VERSION 2.4 )

#| Project
project ( test )

#| Sources
set ( SRCS
	main.cpp
)

#| qtermwidget Library
include_directories (
	${PROJECT_SOURCE_DIR}/../lib
)

set(QTERMWIDGET_LIB qtermwidget)
link_directories(${PROJECT_BINARY_DIR}/../)

#| Qt4 Required Options
add_definitions ( -Wall )
find_package ( Qt4 REQUIRED ) # Finds Qt4 on the system
include ( ${QT_USE_FILE} ) # Includes Qt4 headers and libraries (the above command is needed first)
QT4_WRAP_CPP ( MOC_SRCS ${HDRS} ) # Moc's the headers

if(COMMAND cmake_policy)
      cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

#| Create the Library
add_executable ( test ${SRCS} )

#| Set Build Type
set ( CMAKE_BUILD_TYPE
	#"release"
	"relwithdebinfo" # Default
	#"debug"
	#"debugfull"
)

#| Link Qt4 Libraries
target_link_libraries ( test ${QT_LIBRARIES} )

#| Link qtermwidget Library 
target_link_libraries ( test ${QTERMWIDGET_LIB} ) 

