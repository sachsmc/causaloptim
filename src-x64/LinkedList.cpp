//#include <afx.h>
#include <windows.h>
#include "LinkedList.h"

void CLinkedList_ :: Append (void * p_pData)
{
	if (m_pLastLink == NULL)
		m_pFirstLink = m_pLastLink = new CLink_;
	else
	{
		m_pLastLink-> m_pNextLink = new CLink_;
		m_pLastLink = m_pLastLink-> m_pNextLink;
	}

	m_pLastLink-> m_pNextLink = NULL;
	m_pLastLink-> m_pData = p_pData;
}  /* CLinkedList_ :: Append () */


void * CLinkedList_ :: Dequeue ()
{
	CLink_ *		pOutLink;
					// Link to be removed.

	void *			pData;
					// Data to be returned.

	if (m_pFirstLink == NULL)
		return NULL;

	pOutLink = m_pFirstLink;

	/*
	 * Remove the link from the list.
	 */
	m_pFirstLink = pOutLink-> m_pNextLink;
	if (m_pFirstLink == NULL)
		m_pLastLink = NULL;

	pData = pOutLink-> m_pData;

	delete pOutLink;
	return pData;

}  /* CLinkedList_ :: Dequeue () */


DWORD CLinkedList_ :: Length ()
{
	CLink_ *	pLink;
	DWORD		Length = 0;

	pLink = m_pFirstLink;

	while (pLink)
	{
		Length++;
		pLink = pLink-> m_pNextLink;
	}
	return Length;
}  /* CLinkedList_ :: Length () */

