#ifndef LINKED_LIST_H
#define	LINKED_LIST_H

#include <windows.h>

class CLink_
{

public:

	void *		m_pData;
				// Data referenced by this link.

	CLink_ *	m_pNextLink;
				// Link that follow the current one (NULL for the
				// last link in a list.
					
	CLink_ ()
	{
		m_pData = NULL;
		m_pNextLink = NULL;
	}
};  /* class CLink_ */


class CLinkedList_
{
	CLink_ *		m_pFirstLink;
					// First link in the list (NULL when list is empty).

	CLink_ *		m_pLastLink;
					// Last link in the list (NULL when list is empty).

public:

	CLinkedList_ ()
	{
		m_pFirstLink = NULL;
		m_pLastLink = NULL;
	}

	void Append (void * p_pData);

	void * Dequeue ();

	DWORD Length ();

};  /* class CLinkedList_ */

#endif // LINKED_LIST
